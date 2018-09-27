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

%%%-------------------------------------------------------------------
%% @doc ogonek top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ogonek_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(WORKER(Id, Mod, Args),
        {Id, {Mod, start_link, Args},
         permanent, 5000, worker, [Mod]}).

-define(SUP(Id, Mod, Args),
        {Id, {Mod, start_link, Args},
         permanent, infinity, supervisor, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, WebserverPort} = application:get_env(webserver_port),

    ElliCbArgs = [{mods, [{ogonek_webserver, [{handler, ogonek_webserver}]},
                      {elli_websocket, []}]}],

    Children = [?WORKER(db, ogonek_db, []),
                ?WORKER(mongodb, ogonek_mongo, []),
                ?WORKER(planet_manager, ogonek_planet_manager, []),
                ?WORKER(twitch, ogonek_twitch, []),
                ?SUP(lifecycle_sup, ogonek_user_lifecycle_sup, []),
                ?WORKER(session_manager, ogonek_session_manager, []),
                ?WORKER(webserver, elli, [[{callback, elli_middleware},
                                           {callback_args, ElliCbArgs},
                                           {port, WebserverPort}]])
               ],
    {ok, {{one_for_all, 5, 10}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
