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
    WebserverPort = 8000,
    CbArgs = [{mods, [{ogonek_webserver, [{handler, ogonek_webserver}]},
                      {elli_websocket, []}]}],
    Children = [?WORKER(webserver, elli, [[{callback, elli_middleware},
                                           {callback_args, CbArgs},
                                           {port, WebserverPort}]]),
                ?WORKER(db, ogonek_db, [])
               ],
    {ok, {{one_for_all, 5, 10}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
