-module(ogonek_websocket_api).

-export([init/2,
         info/3,
         request/3
        ]).

-record(state, {}).


init(Request, Opts) ->
    lager:debug("initialize websocket channel: ~p ~p [~p]", [Request, Opts, self()]),
    {ok, [], #state{}}.


info(Request, Message, State) ->
    lager:debug("websocket info: ~p ~p [~p]", [Request, Message, self()]),
    {ok, State}.


request(Request, {text, Msg}, State) ->
    lager:debug("websocket channel: ~p ~p [~p]", [Request, Msg, self()]),
    case parse_json(Msg) of
        {ok, Payload} ->
            handle_request(Request, Payload, State);
        {error, Error} ->
            {reply, error_json(Error), State}
    end;

request(Request, Message, State) ->
    lager:debug("websocket channel: ~p ~p [~p]", [Request, Message, self()]),
    {reply, error_json(<<"expecting text message">>), State}.


%%
%% PRIVATE FUNCTIONS
%%

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


parse_json(Body) ->
    try
        Json = jiffy:decode(Body),
        {ok, Json}
    catch
        _Error -> {error, <<"malformed JSON">>}
    end.
