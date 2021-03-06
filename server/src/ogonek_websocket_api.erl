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

-include("include/ogonek.hrl").

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
                        ogonek_auth:send_auth_infos(self()),

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

    {reply, json(ogonek_user:to_json(User, false)), State#ws_state{user_id=UserId}};

info(_Request, {session_login, SessionId, UserId}, State) ->
    lager:warning("received session_login for session '~s' and user '~s' at websocket of session '~s'",
                  [SessionId, UserId, State#ws_state.session_id]),
    {ok, State};

info(_Request, {dispatcher, Dispatcher}, State) ->
    SessionId = State#ws_state.session_id,
    lager:debug("session ~s - associating with user session dispatcher ~p", [SessionId, Dispatcher]),

    {ok, State#ws_state{dispatcher=Dispatcher}};

info(_Request, {lifecycle, Lifecycle}, State) ->
    SessionId = State#ws_state.session_id,
    lager:debug("session ~s - associating with user lifecycle ~p", [SessionId, Lifecycle]),

    {ok, State#ws_state{lifecycle=Lifecycle}};

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

    case ogonek_mongo:new_session(RemoteIp, Headers) of
        {ok, SessionId} -> SessionId;
        _Otherwise -> <<>>
    end.


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
                                              [{ogonek_util:trim(Key), ogonek_util:trim(Value)} | Cs];
                                          _ -> Cs
                                      end
                              end, [], Values),

            proplists:get_value(<<"ogonekSession">>, KVs)
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
    Keys = [<<"code">>, <<"scope">>, <<"state">>,
            {<<"provider">>, ogonek_auth:default_provider()}],

    case ogonek_util:keys(Keys, Json) of
        [Code, Scope, St, Provider] ->

            case ogonek_auth:provider_from_binary(Provider) of
                error ->
                    {reply, error_json(<<"invalid auth provider">>), State};
                ProviderId ->
                    ProviderModule = ogonek_auth:provider_module(ProviderId),

                    % at first we are going to validate the passed values
                    % against the configured auth service
                    case ProviderModule:auth_user(Code, Scope, St) of
                        {ok, User} ->
                            % on success we are going to connect this session with the
                            % user that is associated with the authorized user
                            ogonek_session_manager:register_socket(User#user.id, State#ws_state.session_id),

                            State0 = State#ws_state{user_id=User#user.id},
                            {reply, json(ogonek_user:to_json(User, false)), State0};
                        Error ->
                            lager:info("authorization failed: ~p", [Error]),

                            % TODO: more specific error response for the client
                            {reply, error_json(<<"authorization failed">>), State}
                    end
            end;
        _Otherwise ->
            % TODO: more specific error response for the client
            {reply, error_json(<<"invalid authorize request">>), State}
    end;

handle_request(<<"planet_info">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"planet_info">>, _Request, _Json, State) ->
    publish_to_user(State, planet_info),
    {ok, State};

handle_request(<<"weapons_info">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"weapons_info">>, _Request, _Json, State) ->
    publish_to_user(State, weapons_info),
    {ok, State};

handle_request(<<"ships_info">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"ships_info">>, _Request, _Json, State) ->
    publish_to_user(State, ships_info),
    {ok, State};

handle_request(<<"get_utilization">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"get_utilization">>, _Request, Json, State) ->
    case ogonek_util:keys([<<"planet">>], Json) of
        [PlanetId] ->
            publish_to_user(State, {get_utilization, PlanetId}),
            {ok, State};
        _Otherwise ->
            {reply, error_json(<<"get_utilization: expecting planet">>), State}
    end;

handle_request(<<"set_utilization">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"set_utilization">>, _Request, Json, State) ->
    case ogonek_util:keys([<<"planet">>, <<"resource">>, <<"value">>], Json) of
        [PlanetId, Resource, Value] when is_integer(Value) ->
            Msg = {set_utilization, PlanetId, Resource, Value},
            publish_to_user(State, Msg),
            {ok, State};
        _Otherwise ->
            {reply, error_json(<<"set_utilization: expecting planet, resource and value">>), State}
    end;

handle_request(<<"start_research">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"start_research">>, _Request, _Json, State) ->
    publish_to_user(State, start_research),
    {ok, State};

handle_request(<<"production_info">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"production_info">>, _Request, Json, State) ->
    case ogonek_util:keys([<<"planet">>], Json) of
        [PlanetId] ->
            publish_to_user(State, {production_info, PlanetId}),
            {ok, State};
        _Otherwise ->
            {reply, error_json(<<"production_info: expecting planet">>), State}
    end;

handle_request(<<"build_building">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"build_building">>, _Request, Json, State) ->
    case ogonek_util:keys([<<"planet">>, <<"type">>, <<"level">>], Json) of
        [Planet, Type, Level] when is_integer(Level) ->
            case ogonek_buildings:try_building_type(Type) of
                {error, invalid} ->
                    {reply, error_json(<<"build_building: invalid type">>), State};
                Type0 ->
                    Msg = {build_building, Planet, Type0, Level},
                    publish_to_user(State, Msg),
                    {ok, State}
            end;
        _Otherwise ->
            {reply, error_json(<<"build_building: expecting planet, type and level">>), State}
    end;

handle_request(<<"build_weapon">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"build_weapon">>, _Request, Json, State) ->
    case ogonek_util:keys([<<"planet">>, <<"weapon">>], Json) of
        [Planet, Weapon] ->
            case ogonek_weapons:try_weapon_type(Weapon) of
                error ->
                    {reply, error_json(<<"build_weapon: invalid weapon">>), State};
                {error, invalid} ->
                    {reply, error_json(<<"build_weapon: invalid weapon">>), State};
                WDef ->
                    Msg = {build_weapon, Planet, WDef},
                    publish_to_user(State, Msg),
                    {ok, State}
            end;
        _Otherwise ->
            {reply, error_json(<<"build_weapon: expecting planet and weapon">>), State}
    end;

handle_request(<<"build_ship">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"build_ship">>, _Request, Json, State) ->
    case ogonek_util:keys([<<"planet">>, <<"ship">>], Json) of
        [Planet, Ship] ->
            case ogonek_ships:try_ship_type(Ship) of
                error ->
                    {reply, error_json(<<"build_ship: invalid ship">>), State};
                {error, invalid} ->
                    {reply, error_json(<<"build_ship: invalid ship">>), State};
                SDef ->
                    Msg = {build_ship, Planet, SDef},
                    publish_to_user(State, Msg),
                    {ok, State}
            end;
        _Otherwise ->
            {reply, error_json(<<"build_ship: expecting planet and ship">>), State}
    end;

handle_request(<<"logout">>, _Request, _Json, #ws_state{user_id=undefined}=State) ->
    not_logged_in(State);

handle_request(<<"logout">>, _Request, _Json, State) ->
    UserId = State#ws_state.user_id,
    ogonek_session_manager:logout(UserId),
    {ok, State};

handle_request(Type, _Request, _Json, State) when is_binary(Type) ->
    {reply, error_json(<<"unhandled request of type '", Type/binary, "'">>), State};

handle_request(_Type, _Request, _Json, State) ->
    {reply, error_json(<<"unhandled request">>), State}.


process_session_id(CookieSessionId, Request) ->
    case ogonek_mongo:get_session(CookieSessionId) of
        {ok, StoredSession} ->
            Socket = self(),
            ogonek_mongo:refresh_session(CookieSessionId),

            % in case the session is associated with a specific user-id
            % we are going to attempt auto-authorization
            case ogonek_session:has_user_id(StoredSession) of
                true ->
                    % we are spawning user authorization asynchronously
                    % since we want to return with the initial connect
                    % as soon as possible
                    erlang:spawn(fun() -> try_auth_user(Socket, StoredSession) end);
                false ->
                    ogonek_auth:send_auth_infos(Socket)
            end,

            CookieSessionId;
        {error, not_found} ->
            % no session cookie yet:
            % - send auth-info to client
            % - create a new session
            ogonek_auth:send_auth_infos(self()),
            request_session(Request)
    end.


-spec try_auth_user(pid(), session()) -> ok.
try_auth_user(Socket, #session{id=Id, user_id=UserId}) ->
    lager:info("validating authorization of user '~s' at session: ~s", [UserId, Id]),

    LoggedIn =
    case ogonek_mongo:get_user(UserId) of
        {ok, #user{oauth=undefined}} ->
            lager:info("user ~p is known - but no oauth information present", [UserId]),
            false;
        {ok, User} ->
            Provider = User#user.provider,
            ProviderModule = ogonek_auth:provider_module(ogonek_auth:provider_from_binary(Provider)),

            case ProviderModule:validate_login(Id, User) of
                {ok, User0} ->
                    Socket ! {session_login, Id, User0},
                    true;
                error ->
                    false
            end;
        _Otherwise ->
            false
    end,

    % send the auth-info to the client if it wasn't logged in
    case LoggedIn of
        false -> ogonek_auth:send_auth_infos(Socket);
        true -> ok
    end.


-spec error_json(binary()) -> {text, binary()}.
error_json(Error) ->
    Payload = {[{<<"error">>, true},
                {?MSG_TYPE, <<"error">>},
                {<<"message">>, Error}]},
    json(Payload).


-spec not_logged_in(ws_state()) -> {reply, {text, binary()}, ws_state()}.
not_logged_in(State) ->
    {reply, error_json(<<"not logged in at all">>), State}.


-spec json(any()) -> {text, binary()}.
json(Payload) ->
    Encoded = jiffy:encode(Payload),
    {text, Encoded}.


-spec publish_to_user(ws_state(), Msg :: term()) -> ok.
publish_to_user(State, Msg) ->
    case State#ws_state.lifecycle of
        undefined ->
            % no associated user lifecycle (yet):
            % publish via global session manager
            UserId = State#ws_state.user_id,
            SessionId = State#ws_state.session_id,
            ogonek_session_manager:publish_to_user(UserId, SessionId, Msg);
        Lifecycle ->
            % directly send to lifecycle otherwise
            Lifecycle ! Msg
    end.
