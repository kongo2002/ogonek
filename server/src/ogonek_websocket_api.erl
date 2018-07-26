-module(ogonek_websocket_api).

-export([init/2,
         info/3,
         request/3
        ]).

-record(state, {}).


init(Request, Opts) ->
    lager:debug("initialize websocket channel: ~p ~p [~p]", [Request, Opts, self()]),

    % TODO: not sure about this delayed send at all
    send_auth_info(self(), 1000),

    Session = case get_cookie_session(Request) of
                  undefined ->
                      RemoteIp = elli_request:peer(Request),
                      Headers = elli_request:headers(Request),
                      lager:debug("~p ~p", [RemoteIp, Headers]),
                      {ok, Id, _Rev} = ogonek_db:new_session(RemoteIp, Headers),
                      Id;
                  CookieSession ->
                      % TODO: check session
                      CookieSession
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
    lager:debug("websocket channel: ~p ~p [~p]", [Request, Msg, self()]),
    case ogonek_util:parse_json(Msg) of
        {ok, Payload} ->
            handle_request(Request, Payload, State);
        {error, malformed_json} ->
            {reply, error_json(<<"malformed JSON">>), State}
    end;

request(Request, Message, State) ->
    lager:debug("websocket channel: ~p ~p [~p]", [Request, Message, self()]),
    {reply, error_json(<<"expecting text message">>), State}.


%%
%% PRIVATE FUNCTIONS
%%

get_cookie_session(Request) ->
    % XXX: does this work with lowercase 'cookie' as well?
    case elli_request:get_header(<<"Cookie">>, Request) of
        undefined -> undefined;
        Cookie ->
            % TODO: improve cookie parsing
            Values = binary:split(Cookie, <<";">>),
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
                         Target ! {json, {[{<<"_t">>, <<"auth">>},
                                           {<<"provider">>, <<"twitch">>},
                                           {<<"loginUrl">>, <<"https://twitch.tv/">>}
                                          ]}}
                 end),
    ok.


handle_request(_Request, _Json, State) ->
    {reply, error_json(<<"unhandled request">>), State}.


error_json(Error) ->
    Payload = {[{<<"error">>, true},
                {<<"_t">>, <<"error">>},
                {<<"message">>, Error}]},
    json(Payload).


json(Payload) ->
    Encoded = jiffy:encode(Payload),
    {text, Encoded}.
