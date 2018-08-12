% Copyright 2018 Gregor Uhlenheuer
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(ogonek_websocket_api).

-include("ogonek.hrl").

-export([init/2,
         info/3,
         request/3
        ]).


init(Request, Opts) ->
    lager:debug("initialize websocket channel: ~p ~p [~p]", [Request, Opts, self()]),

    SessionId = case get_cookie_session(Request) of
                    undefined ->
                        % there is no session stored at all
                        % so we have to send auth-info to the client for sure
                        send_auth_info(self()),

                        % moreover create a new session
                        request_session(Request);
                    CookieSessionId ->
                        process_session_id(CookieSessionId, Request)
                end,

    % set or update session-id in client cookie (valid for 7 days at max)
    AdditionalHeaders = [{<<"Set-Cookie">>,
                          <<"ogonekSession=", SessionId/binary, "; Max-Age=604800; HttpOnly">>}],

    {ok, AdditionalHeaders, #ws_state{session_id=SessionId}}.


info(_Request, {json, Json}, State) ->
    {reply, json(Json), State};

info(_Request, {session_login, SessionId, User}, #ws_state{session_id=SessionId}=State) ->
    UserId = User#user.id,
    lager:info("user '~s' successfully logged in via session '~s'", [UserId, SessionId]),

    ogonek_session_manager:register_socket(UserId, SessionId),

    {reply, json(ogonek_user:to_json(User)), State#ws_state{user_id=UserId}};

info(_Request, {session_login, SessionId, UserId}, State) ->
    lager:warning("received session_login for session '~s' and user '~s' at websocket of session '~s'",
                  [SessionId, UserId, State#ws_state.session_id]),
    {ok, State};

info(_Request, {logout, Reason}, State) ->
    lager:info("received logout request with reason: ~p", [Reason]),
    {shutdown, State};

info(Request, Message, State) ->
    lager:warning("websocket unhandled info: ~p ~p [~p]", [Request, Message, State]),
    {ok, State}.


request(Request, {text, Msg}, State) ->
    lager:debug("websocket request: ~p [~p]", [Msg, State]),

    case ogonek_util:parse_json(Msg) of
        {ok, Payload} ->
            handle_json(Request, Payload, State);
        {error, malformed_json} ->
            {reply, error_json(<<"malformed JSON">>), State}
    end;

request(_Request, Message, State) ->
    lager:debug("websocket channel: ~p [~p]", [Message, self()]),
    {reply, error_json(<<"expecting text message">>), State}.


%%
%% PRIVATE FUNCTIONS
%%

request_session(Request) ->
    RemoteIp = elli_request:peer(Request),
    Headers = elli_request:headers(Request),
    {ok, SessionId, _Rev} = ogonek_db:new_session(RemoteIp, Headers),
    SessionId.


get_cookie_session(Request) ->
    % XXX: does this work with lowercase 'cookie' as well?
    case elli_request:get_header(<<"Cookie">>, Request) of
        undefined -> undefined;
        Cookie ->
            % TODO: improve cookie parsing
            Values = binary:split(Cookie, <<";">>, [trim_all, global]),
            KVs = lists:foldl(fun(V, Cs) ->
                                      case binary:split(V, <<"=">>) of
                                          [Key, Value] ->
                                              [{string:trim(Key), string:trim(Value)} | Cs];
                                          _ -> Cs
                                      end
                              end, [], Values),

            proplists:get_value(<<"ogonekSession">>, KVs)
    end.


send_auth_info(Target) ->
    case ogonek_twitch:get_info() of
        {ok, AuthInfo} ->
            Target ! {json, AuthInfo};
        _Otherwise ->
            % do nothing
            ok
    end.


handle_json(Request, {Json}, State) ->
    case proplists:get_value(?MSG_TYPE, Json) of
        undefined ->
            {reply, error_json(<<"missing type field 't'">>), State};
        Type ->
            handle_request(Type, Request, Json, State)
    end;

handle_json(_Request, _Json, State) ->
    {reply, error_json(<<"invalid JSON received - expecting JSON object">>), State}.


handle_request(<<"authorize">>, _Request, Json, State) ->
    % the values 'code', 'scope' and 'state' are passed to the
    % callback uri of the oauth procedure and sent via websocket
    % to this authorize API
    case ogonek_util:keys([<<"code">>, <<"scope">>, <<"state">>], Json) of
        [Code, Scope, St] ->
            % at first we are going to validate the passed values
            % against the configured auth service (twitch for now)
            case auth_user(Code, Scope, St) of
                {ok, User} ->
                    % on success we are going to connect this session with the
                    % user that is associated with the authorized user
                    ogonek_session_manager:register_socket(User#user.id, State#ws_state.session_id),

                    State0 = State#ws_state{user_id=User#user.id},
                    {reply, json(ogonek_user:to_json(User)), State0};
                _Error ->
                    % TODO: more specific error response for the client
                    {reply, error_json(<<"authorization failed">>), State}
            end;
        _Otherwise ->
            % TODO: more specific error response for the client
            {reply, error_json(<<"invalid authorize request">>), State}
    end;

handle_request(<<"logout">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    {reply, error_json(<<"not logged in at all">>), State};

handle_request(<<"logout">>, _Request, _Json, State) ->
    UserId = State#ws_state.user_id,
    ogonek_session_manager:logout(UserId),
    {ok, State};

handle_request(Type, _Request, _Json, State) when is_binary(Type) ->
    {reply, error_json(<<"unhandled request of type '", Type/binary, "'">>), State};

handle_request(_Type, _Request, _Json, State) ->
    {reply, error_json(<<"unhandled request">>), State}.


process_session_id(CookieSessionId, Request) ->
    case ogonek_db:get_session(CookieSessionId) of
        {ok, StoredSession} ->
            Socket = self(),
            ogonek_db:refresh_session(CookieSessionId),

            % in case the session is associated with a specific user-id
            % we are going to attempt auto-authorization
            case ogonek_session:has_user_id(StoredSession) of
                true ->
                    % we are spawning user authorization asynchronously
                    % since we want to return with the initial connect
                    % as soon as possible
                    erlang:spawn(fun() -> try_auth_user(Socket, StoredSession) end);
                false ->
                    send_auth_info(Socket)
            end,

            CookieSessionId;
        {error, not_found} ->
            % no session cookie yet:
            % - send auth-info to client
            % - create a new session
            send_auth_info(self()),
            request_session(Request)
    end.


-spec try_auth_user(pid(), session()) -> ok.
try_auth_user(Socket, #session{id=Id, user_id=UserId}) ->
    lager:info("validating authorization of user '~s' at session: ~s", [UserId, Id]),

    LoggedIn =
    case ogonek_db:get_user(UserId) of
        {ok, #user{oauth=undefined}} ->
            false;
        {ok, User} ->
            OAuth = User#user.oauth,
            ProviderId = User#user.provider_id,
            case ogonek_twitch:validate_token(OAuth#oauth_access.access_token) of
                {ok, ProviderId} ->
                    % notify owning socket of successful session login
                    Socket ! {session_login, Id, User},
                    true;
                _Otherwise ->
                    case ogonek_twitch:refresh_token(User) of
                        {ok, RefreshOAuth} ->
                            WithAuth = User#user{oauth=RefreshOAuth},
                            ogonek_db:update_user(WithAuth),
                            Socket ! {session_login, Id, User},
                            true;
                        error -> false
                    end
            end;
        _Otherwise ->
            false
    end,

    % send the auth-info to the client if it wasn't logged in
    case LoggedIn of
        false -> send_auth_info(Socket);
        true -> ok
    end.


-spec auth_user(binary(), binary(), binary()) ->
    {ok, user()} |
    {error, invalid} |
    {error, missing_id} |
    {error, missing_rev} |
    {error, authorization_failed}.
auth_user(Code, Scope, StateStr) ->
    lager:debug("trying to authorize with: [code ~p; scope ~p; state ~p]", [Code, Scope, StateStr]),

    % TODO: abstract twitch specifics into a 'generic' auth-provider
    case ogonek_twitch:get_auth_token(Code) of
        {ok, Token} ->
            Provider = <<"twitch">>,
            case ogonek_twitch:get_user(Token) of
                {ok, TwitchUser} ->
                    case ogonek_db:get_user(TwitchUser#twitch_user.id, Provider) of
                        {ok, Existing} ->
                            WithAuth = Existing#user{oauth=Token},
                            ogonek_db:update_user(WithAuth),
                            {ok, WithAuth};
                        {error, not_found} ->
                            ogonek_db:create_user_from_twitch(TwitchUser, Provider)
                    end;
                Error ->
                    lager:warning("twitch user request failed: ~p", [Error]),
                    {error, authorization_failed}
            end;
        Error ->
            lager:warning("twitch authorization failed: ~p", [Error]),
            {error, authorization_failed}
    end.


-spec error_json(binary()) -> {text, binary()}.
error_json(Error) ->
    Payload = {[{<<"error">>, true},
                {?MSG_TYPE, <<"error">>},
                {<<"message">>, Error}]},
    json(Payload).


-spec json(any()) -> {text, binary()}.
json(Payload) ->
    Encoded = jiffy:encode(Payload),
    {text, Encoded}.
