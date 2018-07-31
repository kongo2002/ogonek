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

-record(state, {
          session_id :: binary()
         }).


init(Request, Opts) ->
    lager:debug("initialize websocket channel: ~p ~p [~p]", [Request, Opts, self()]),

    % TODO: not sure about this delayed send at all
    send_auth_info(self(), 1000),

    SessionId = case get_cookie_session(Request) of
                    undefined ->
                        request_session(Request);
                    CookieSessionId ->
                        case ogonek_db:get_session(CookieSessionId) of
                            {ok, _StoredSession} ->
                                ogonek_db:refresh_session(CookieSessionId),
                                CookieSessionId;
                            {error, not_found} ->
                                request_session(Request)
                        end
                end,

    % set or update session-id in client cookie (valid for 7 days at max)
    AdditionalHeaders = [{<<"Set-Cookie">>,
                          <<"ogonekSession=", SessionId/binary, "; Max-Age=604800; HttpOnly">>}],

    {ok, AdditionalHeaders, #state{session_id=SessionId}}.


info(_Request, {json, Json}, State) ->
    {reply, json(Json), State};

info(_Request, {logout, Reason}, State) ->
    lager:info("received logout request with reason: ~p", [Reason]),
    {reply, json({[{?MSG_TYPE, logout}]}), State};

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


send_auth_info(Target, Delay) ->
    erlang:spawn(fun() ->
                         timer:sleep(Delay),
                         case ogonek_twitch:get_info() of
                             {ok, AuthInfo} ->
                                 Target ! {json, AuthInfo};
                             _Otherwise ->
                                 % do nothing
                                 ok
                         end
                 end),
    ok.


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
                    ogonek_session_manager:register(User#user.id, State#state.session_id),
                    {reply, json(ogonek_user:to_json(User)), State};
                _Error ->
                    % TODO: more specific error response for the client
                    {reply, error_json(<<"authorization failed">>), State}
            end;
        _Otherwise ->
            % TODO: more specific error response for the client
            {reply, error_json(<<"invalid authorize request">>), State}
    end;

handle_request(Type, _Request, _Json, State) when is_binary(Type) ->
    {reply, error_json(<<"unhandled request of type '", Type/binary, "'">>), State};

handle_request(_Type, _Request, _Json, State) ->
    {reply, error_json(<<"unhandled request">>), State}.


auth_user(Code, Scope, StateStr) ->
    lager:debug("trying to authorize with: [code ~p; scope ~p; state ~p]", [Code, Scope, StateStr]),

    % TODO: abstract twitch specifics into a 'generic' auth-provider
    case ogonek_twitch:get_auth_token(Code) of
        {ok, Token} ->
            Provider = <<"twitch">>,
            case ogonek_twitch:get_user(Token) of
                {ok, TwitchUser} ->
                    case ogonek_db:get_user(TwitchUser#twitch_user.id, Provider) of
                        {ok, _}=Existing -> Existing;
                        {error, not_found} ->
                            ogonek_db:create_user(TwitchUser, Provider)
                    end;
                Error ->
                    lager:warning("twitch user request failed: ~p", [Error]),
                    {error, authorization_failed}
            end;
        Error ->
            lager:warning("twitch authorization failed: ~p", [Error]),
            {error, authorization_failed}
    end.


error_json(Error) ->
    Payload = {[{<<"error">>, true},
                {?MSG_TYPE, <<"error">>},
                {<<"message">>, Error}]},
    json(Payload).


json(Payload) ->
    Encoded = jiffy:encode(Payload),
    {text, Encoded}.
