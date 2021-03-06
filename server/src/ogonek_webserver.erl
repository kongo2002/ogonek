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

-module(ogonek_webserver).

-include("include/ogonek.hrl").

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


% serve root file
handle('GET', [], Req, Args) ->
    handle('GET', [<<"index.html">>], Req, Args);

% websocket upgrade request
handle('websocket', [<<".ws">>], Req, Args) ->
    elli_websocket:upgrade(Req, Args),
    {close, <<>>};

% websocket request w/o upgrade
handle('GET', [<<".ws">>], _Req, _Args) ->
    {200, [], <<"use an upgrade request">>};

% arbitrary *static* file request
handle('GET', [<<"static">> | _]=Path, _Req, _Args) ->
    serve_file(Path);

% favicon
handle('GET', [<<"favicon.ico">>], _Req, _Args) ->
    {404, <<"not found">>};

% all remaining GET request will be mapped to '/index.html'
handle('GET', _Path, _Req, _Args) ->
    serve_file([<<"index.html">>]);

% everything else 'does not exist'
handle(_, _Path, _Req, _Args) ->
    {404, <<"not found">>}.

%%
%% WEBSOCKET CALLBACKS
%%

websocket_init(Req, Opts) ->
    ogonek_websocket_api:init(Req, Opts).


websocket_info(Req, Message, State) ->
    ogonek_websocket_api:info(Req, Message, State).


websocket_handle(Req, Message, State) ->
    ogonek_websocket_api:request(Req, Message, State).

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

websocket_handle_event(websocket_close, [Request, _Reason], #ws_state{user_id=UserId}) ->
    lager:debug("closing websocket: ~p", [Request]),
    ogonek_session_manager:close_socket(UserId),
    ok;
websocket_handle_event(websocket_close, [Request, _Reason], _State) ->
    lager:debug("closing websocket: ~p", [Request]),
    ok;

%% websocket_throw, websocket_error and websocket_exit events are sent if
%% the user callback code throws an exception, has an error or
%% exits. After triggering this event, a generated response is sent to
%% the user.
websocket_handle_event(websocket_throw, [_Request, Exception, Stacktrace], _) ->
    lager:error("throw on websocket: ~p ~p", [Exception, Stacktrace]),
    ok;
websocket_handle_event(websocket_error, [_Request, Exception, Stacktrace], _) ->
    lager:error("error on websocket: ~p ~p", [Exception, Stacktrace]),
    ok;
websocket_handle_event(websocket_exit, [_Request, Exception, Stacktrace], _) ->
    lager:error("exit on websocket: ~p ~p", [Exception, Stacktrace]),
    ok.


%%
%% PRIVATE FUNCTIONS
%%

serve_file(Path) ->
    Filepath = filename:join([?DOCROOT | Path]),
    valid_path(Filepath) orelse throw({403, [], <<"permission denied">>}),

    case file:read_file(Filepath) of
        {ok, Bin} ->
            {ok, [{<<"Cache-Control">>, <<"max-age: 3600">>}], Bin};
        {error, enoent} ->
            {404, <<"not found">>};
        {error, eisdir} ->
            {404, <<"not found">>}
    end.


init_ws([<<".ws">>], _Req, _Args) ->
    {ok, handover};
init_ws(_, _, _) ->
    ignore.


valid_path(Path) ->
    case binary:match(Path, <<"..">>) of
        {_, _} -> false;
        nomatch -> true
    end.
