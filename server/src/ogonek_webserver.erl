-module(ogonek_webserver).

-export([init/2,
         handle/2,
         handle_event/3]).

-export([websocket_init/2,
         websocket_info/3,
         websocket_handle/3,
         websocket_handle_event/3
        ]).

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).
-behaviour(elli_websocket_handler).

-define(DOCROOT, <<"assets">>).


init(Req, Args) ->
    case elli_request:get_header(<<"Upgrade">>, Req) of
        <<"websocket">> ->
            init_ws(elli_request:path(Req), Req, Args);
        _ ->
            ignore
    end.


handle(Req, Args) ->
    Method = case elli_request:get_header(<<"Upgrade">>, Req) of
                 <<"websocket">> ->
                     websocket;
                 _ ->
                     elli_request:method(Req)
             end,
    handle(Method, elli_request:path(Req), Req, Args).


handle('GET', [], Req, Args) ->
    handle('GET', [<<"index.html">>], Req, Args);

handle('websocket', [<<".ws">>], Req, Args) ->
    elli_websocket:upgrade(Req, Args),
    {close, <<>>};

handle('GET', [<<".ws">>], _Req, _Args) ->
    {200, [], <<"use an upgrade request">>};

% arbitrary file request
handle('GET', Path, _Req, _Args) ->
    Filepath = filename:join([?DOCROOT | Path]),
    valid_path(Filepath) orelse throw({403, [], <<"permission denied">>}),

    case file:read_file(Filepath) of
        {ok, Bin} ->
            {ok, Bin};
        {error, enoent} ->
            {404, <<"not found">>};
        {error, eisdir} ->
            {404, <<"not found">>}
    end;

handle(_, _Path, _Req, _Args) ->
    {404, <<"not found">>}.

%%
%% WEBSOCKET CALLBACKS
%%

websocket_init(Req, Opts) ->
    lager:debug("ws_init: ~p, ~p", [Req, Opts]),
    State = undefined,
    {ok, [], State}.

websocket_info(_Req, Message, State) ->
    lager:debug("ws_info: ~p", [Message]),
    {ok, State}.

websocket_handle(_Req, Message, State) ->
    lager:debug("ws_handle: ~p", [Message]),
    %%  default behaviour.
    {ok, State}.

%%
%% ELLI EVENT CALLBACKS
%%

handle_event(request_throw, [Req, Exception, Stack], _Config) ->
    lager:error("exception: ~p stack: ~p request: ~p",
                [Exception, Stack, elli_request:to_proplist(Req)]),
    ok;
handle_event(request_exit, [Req, Exit, Stack], _Config) ->
    lager:error("exit: ~p stack: ~p request: ~p",
                [Exit, Stack, elli_request:to_proplist(Req)]),
    ok;

handle_event(request_error, [Req, Error, Stack], _Config) ->
    lager:error("error: ~p stack: ~p request: ~p",
                [Error, Stack, elli_request:to_proplist(Req)]),
    ok;

handle_event(_Event, _Data, _Args) ->
    ok.

%%
%% ELLI WEBSOCKET EVENT CALLBACKS
%%

%% websocket_open and websocket_close events are sent when the websocket
%% opens, and when it closes.
websocket_handle_event(websocket_open, [_, _Version, _Compress], _) -> ok;
websocket_handle_event(websocket_close, [_, _Reason], _) -> ok;

%% websocket_throw, websocket_error and websocket_exit events are sent if
%% the user callback code throws an exception, has an error or
%% exits. After triggering this event, a generated response is sent to
%% the user.
websocket_handle_event(websocket_throw, [_Request, _Exception, _Stacktrace], _) -> ok;
websocket_handle_event(websocket_error, [_Request, _Exception, _Stacktrace], _) -> ok;
websocket_handle_event(websocket_exit, [_Request, _Exception, _Stacktrace], _) -> ok.


%%
%% PRIVATE FUNCTIONS
%%

init_ws([<<".ws">>], _Req, _Args) ->
    {ok, handover};
init_ws(_, _, _) ->
    ignore.


valid_path(Path) ->
    case binary:match(Path, <<"..">>) of
        {_, _} -> false;
        nomatch -> true
    end.
