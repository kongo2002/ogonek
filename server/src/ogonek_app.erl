%%%-------------------------------------------------------------------
%% @doc ogonek public API
%% @end
%%%-------------------------------------------------------------------

-module(ogonek_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    % logging
    lager:start(),

    ok = application:ensure_started(crypto),
    ok = application:ensure_started(hackney),

    ogonek_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
