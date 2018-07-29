-module(ogonek_websocket_api).

-export([init/2,
         info/3,
         request/3
        ]).

-record(state, {}).

-define(MSG_TYPE, <<"_t">>).


init(Request, Opts) ->
    lager:debug("initialize websocket channel: ~p ~p [~p]", [Request, Opts, self()]),

    % TODO: not sure about this delayed send at all
    send_auth_info(self(), 1000),

    Session = case get_cookie_session(Request) of
                  undefined ->
                      request_session(Request);
                  CookieSession ->
                      case ogonek_db:get_session(CookieSession) of
                          ok ->
                              ogonek_db:refresh_session(CookieSession),
                              CookieSession;
                          {error, not_found} ->
                              request_session(Request)
                      end
              end,

    AdditionalHeaders = [{<<"Set-Cookie">>,
                          <<"ogonekSession=", Session/binary, "; Max-Age=604800; HttpOnly">>}],

    {ok, AdditionalHeaders, #state{}}.


info(_Request, {json, Json}, State) ->
    {reply, json(Json), State};

info(Request, Message, State) ->
    lager:warning("websocket unhandled info: ~p ~p [~p]", [Request, Message, self()]),
    {ok, State}.


request(Request, {text, Msg}, State) ->
    lager:debug("websocket channel: ~p [~p]", [Msg, self()]),
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
            {reply, error_json(<<"missing type field '_t'">>), State};
        Type ->
            handle_request(Type, Request, Json, State)
    end;

handle_json(_Request, _Json, State) ->
    {reply, error_json(<<"invalid JSON received - expecting JSON object">>), State}.


handle_request(<<"authorize">>, _Request, Json, State) ->
    case ogonek_util:keys([<<"code">>, <<"scope">>, <<"state">>], Json) of
        [Code, Scope, St] ->
            lager:info("authorize: [code ~p; scope ~p; state ~p]", [Code, Scope, St]),

            % TODO: actually handle auth token
            case ogonek_twitch:get_auth_token(Code) of
                {ok, Token} ->
                    User = ogonek_twitch:get_user(Token),
                    lager:info("twitch user authenticated: ~p", [User]);
                _Error ->
                    ok
            end,

            {reply, json({[{<<"todo">>, true}]}), State};
        _Otherwise ->
            {reply, error_json(<<"invalid authorize request">>), State}
    end;

handle_request(Type, _Request, _Json, State) when is_binary(Type) ->
    {reply, error_json(<<"unhandled request of type '", Type/binary, "'">>), State};

handle_request(_Type, _Request, _Json, State) ->
    {reply, error_json(<<"unhandled request">>), State}.


error_json(Error) ->
    Payload = {[{<<"error">>, true},
                {?MSG_TYPE, <<"error">>},
                {<<"message">>, Error}]},
    json(Payload).


json(Payload) ->
    Encoded = jiffy:encode(Payload),
    {text, Encoded}.
