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

-module(ogonek_user_lifecycle).

-include("include/ogonek.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {
          id :: binary(),
          planets :: #{binary() => pid()},
          session :: pid(),
          research :: [research()],
          research_timer :: undefined | reference()
         }).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(UserId, SessionDispatcher) ->
    gen_server:start_link(?MODULE, [UserId, SessionDispatcher], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([UserId, SessionDispatcher]) ->
    lager:info("initializing user lifecycle of '~s' [~p]", [UserId, self()]),
    State = #state{
               id=UserId,
               planets=maps:new(),
               research=[],
               session=SessionDispatcher},

    gen_server:cast(self(), prepare),

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(prepare, #state{id=UserId}=State) ->
    lager:debug("preparing user lifecycle of '~s'", [UserId]),

    Self = self(),
    Session = State#state.session,
    Planets = fetch_planets(State),
    PlanetMap = lists:foldl(fun(#planet{id=Id}=Planet, Ps) ->
                                    {ok, UserPlanet} = ogonek_user_planet:start_link(Planet, Session),
                                    maps:put(Id, UserPlanet, Ps)
                            end, maps:new(), Planets),

    lager:debug("user '~s' has ~p planets: ~p",
                [UserId, maps:size(PlanetMap), maps:keys(PlanetMap)]),


    % fetch research progress
    Self ! get_research,
    Self ! unlock_buildings,

    {noreply, State#state{planets=PlanetMap}};

handle_cast({terminate, Reason}, #state{id=UserId}=State) ->
    lager:info("user ~s - request to terminate [reason ~p]", [UserId, Reason]),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({get_planets, Sender}, State) ->
    Planets = maps:keys(State#state.planets),
    Sender ! {planets, Planets},
    {noreply, State};

handle_info(planet_info, State) ->
    for_all_planets(planet_info, State),

    self() ! get_research,

    {noreply, State};

handle_info(weapons_info, State) ->
    for_all_planets(weapons_info, false, State),
    {noreply, State};

handle_info(ships_info, State) ->
    for_all_planets(ships_info, false, State),
    {noreply, State};

handle_info(start_research, State) ->
    Duration = research_duration(State),

    case {current_research(State#state.research), Duration} of
        {_, undefined} ->
            % research not possible (yet)
            {noreply, State};
        {{undefined, Research}, _Duration} ->
            FinishedAt = ogonek_util:finished_at(Duration),
            Possible = ogonek_research:possible_research(Research),
            Pick = ogonek_util:choose_random(Possible),

            Res =
            case get_research(Research, Pick#rdef.name) of
                {ok, Res0} ->
                    % update research
                    Res0#research{
                      level=Res0#research.level+1,
                      created=ogonek_util:now8601(),
                      finish=FinishedAt,
                      progress=true};
                _Otherwise ->
                    % new research
                    #research{
                       user=State#state.id,
                       research=Pick#rdef.name,
                       level=1,
                       created=ogonek_util:now8601(),
                       finish=FinishedAt,
                       progress=true
                      }
            end,

            lager:info("user ~s - starting research: ~p", [State#state.id, Res]),

            ogonek_mongo:research_create(Res),
            Rss = update_research(State#state.research, Res),

            Timer = trigger_research_check(Res),

            {noreply, State#state{research=Rss, research_timer=Timer}};
        _Otherwise ->
            % research already ongoing
            lager:warning("user ~s - starting research impossible - research ongoing", [State#state.id]),
            {noreply, State}
    end;

handle_info({research_create, Research}, State) ->
    Rss = update_research(State#state.research, Research),
    self() ! get_research,
    {noreply, State#state{research=Rss}};

handle_info({research_finish, _Research}, State) ->
    self() ! get_research,
    self() ! unlock_buildings,

    % reset research timer
    State0 = State#state{research_timer=undefined},

    {noreply, State0};

handle_info(unlock_buildings, State) ->
    Researches = State#state.research,

    for_all_planets({unlock_buildings, Researches}, State),

    {noreply, State};

handle_info({build_building, PlanetId, Type, Level}, State) ->
    Msg = {build_building, State#state.research, Type, Level},
    to_planet(PlanetId, Msg, State),
    {noreply, State};

handle_info({build_weapon, PlanetId, WDef}, State) ->
    to_planet(PlanetId, {build_weapon, WDef}, State),
    {noreply, State};

handle_info({production_info, PlanetId}, State) ->
    to_planet(PlanetId, production_info, State),
    {noreply, State};

handle_info({get_utilization, PlanetId}, State) ->
    to_planet(PlanetId, get_utilization, State),
    {noreply, State};

handle_info({set_utilization, PlanetId, Resource, Value}, State) ->
    to_planet(PlanetId, {set_utilization, Resource, Value}, State),
    {noreply, State};

handle_info(get_research, State) ->
    Now = ogonek_util:now8601(),
    Research = fetch_research(State),

    {Pending, Finished, Research0} =
    case current_research(Research) of
        {undefined, Finished0} ->
            % no ongoing research at all
            {undefined, Finished0, Research};
        {Pending0, Finished0} when Pending0#research.finish =< Now ->
            % a research was finished in the meantime:
            % unset progress and move into finished-set
            Pending1 = Pending0#research{progress=false},

            ogonek_mongo:research_finish(Pending1),

            Updated = update_research(Finished0, Pending1),
            {undefined, Updated, Updated};
        {P, F} ->
            % there is a pending research that is not finished yet
            {P, F, Research}
    end,

    % refresh research timer if necessary
    State0 =
    case {Pending, State#state.research_timer} of
        {#research{}, undefined} ->
            State#state{research_timer=trigger_research_check(Pending)};
        {#research{}, TimerRef} ->
            erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
            State#state{research_timer=trigger_research_check(Pending)};
        _Otherwise ->
            State
    end,

    ResearchDuration = research_duration(State),
    Json = ogonek_research:research_info_json(Pending, Finished, ResearchDuration),
    gen_server:cast(State0#state.session, {json_to_sockets, Json}),

    State1 = State0#state{research=Research0},

    {noreply, State1};

handle_info({planet_claim, Planet}=Claim, State) ->
    PlanetId = Planet#planet.id,
    case maps:get(PlanetId, State#state.planets, undefined) of
        undefined ->
            Planet0 = bootstrap_free_planet(Planet),

            ogonek_mongo:planet_update_resources(PlanetId, Planet0#planet.resources),

            {ok, Pid} = ogonek_user_planet:start_link(Planet0, State#state.session),
            Planets0 = maps:put(PlanetId, Pid, State#state.planets),
            {noreply, State#state{planets=Planets0}};
        Pid ->
            Pid ! Claim,
            {noreply, State}
    end;

handle_info(Info, #state{id=Id}=State) ->
    lager:warning("user ~s - unhandled message: ~p", [Id, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    % terminate child planet actors
    Terminate = {terminate, Reason},
    Planets = maps:values(State#state.planets),
    lists:foreach(fun(Pid) -> gen_server:cast(Pid, Terminate) end, Planets),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec fetch_planets(state()) -> [planet()].
fetch_planets(State) ->
    UserId = State#state.id,
    Planets = ogonek_planet_manager:user_planets(UserId),

    case Planets of
        [] ->
            lager:info("user ~s has no planets yet - assigning a free one now", [UserId]),

            % TODO: race until the new planet is claimed
            ogonek_planet_manager:claim_free_planet(UserId),
            [];
        Ps -> Ps
    end.


-spec fetch_research(state()) -> [research()].
fetch_research(State) ->
    case State#state.research of
        [] -> ogonek_mongo:research_of_user(State#state.id);
        Research -> Research
    end.


-spec current_research([research()]) -> {research() | undefined, [research()]}.
current_research(Research) ->
    InProgress = fun(R) -> R#research.progress end,

    case lists:partition(InProgress, Research) of
        {[], Finished} ->
            {undefined, Finished};
        {[Pending], Finished} when Pending#research.level > 1 ->
            Pending0 = Pending#research{level=Pending#research.level-1},
            {Pending, update_research(Finished, Pending0)};
        {[Pending], Finished} ->
            {Pending, Finished};
        {MultiplePending, Finished} ->
            lager:error("multiple pending researches: ~p", [MultiplePending]),
            {hd(MultiplePending), Finished}
    end.


-spec trigger_research_check(research()) -> reference().
trigger_research_check(Research) ->
    Progress = ogonek_research:progress(Research),
    DueIn = ogonek_util:seconds_since(Research#research.finish) + 1,

    ScheduleIn = if Progress < 50 ->
                        round((DueIn * (50 - Progress) / 100) + 10);
                    true ->
                        DueIn + 1
                 end,

    lager:debug("user ~s - scheduling research timer in ~p sec",
                [Research#research.user, ScheduleIn]),

    erlang:send_after(ScheduleIn * 1000, self(), get_research).


-spec get_research([research()], atom()) -> {ok, research()} | undefined.
get_research(Rss, Type) ->
    case lists:keyfind(Type, 4, Rss) of
        false -> undefined;
        Research -> {ok, Research}
    end.


-spec update_research([research()], research()) -> [research()].
update_research(Researches, #research{research=Name}=Research) ->
    case get_research(Researches, Name) of
        undefined -> [Research | Researches];
        _Otherwise ->
            lists:keyreplace(Name, 4, Researches, Research)
    end.


-spec research_duration(state()) -> integer() | undefined.
research_duration(State) ->
    Buildings = all_buildings(State),
    Duration = ogonek_research:research_duration(Buildings),

    case Duration of
        undefined -> undefined;
        _ -> round(Duration / ?OGONEK_DEFAULT_ACCELERATION)
    end.


-spec all_buildings(state()) -> [building()].
all_buildings(State) ->
    Pids = maps:values(State#state.planets),
    lists:flatmap(fun(Pid) ->
                          gen_server:call(Pid, get_buildings)
                  end, Pids).


-spec for_all_planets(Msg :: term(), state()) -> ok.
for_all_planets(Msg, #state{planets=Planets}) ->
    lists:foreach(fun(P) -> P ! Msg end, maps:values(Planets)).


-spec for_all_planets(Msg :: term(), Silent :: boolean(), state()) -> ok.
for_all_planets(Msg, Silent, #state{planets=Planets}) ->
    lists:foreach(fun(P) -> P ! {Msg, Silent} end, maps:values(Planets)).


-spec to_planet(PlanetId :: binary(), Msg :: term(), state()) -> ok.
to_planet(PlanetId, Msg, State) ->
    case maps:get(PlanetId, State#state.planets, undefined) of
        undefined ->
            ok;
        Pid ->
            Pid ! Msg
    end.


-spec bootstrap_free_planet(planet()) -> planet().
bootstrap_free_planet(Planet) ->
    lager:info("user ~s - bootstrapping initial infrastructure on planet ~s",
               [Planet#planet.owner, Planet#planet.id]),

    % this is a new free planet: let's populate
    % with initial set of infrastructure
    PlanetId = Planet#planet.id,

    InitialBuildings = [% initial set of buildings
                        {construction_center, 1},
                        {oil_rig, 1},
                        {water_rig, 1},
                        {ore_mine, 1},
                        {gold_mine, 1},
                        {oil_tank, 1},
                        {water_tank, 1},
                        {ore_depot, 1},
                        {gold_depot, 1},
                        {power_plant, 1},
                        {apartment_block, 1},
                        % remaining buildings that are not built yet
                        {apartment, 0},
                        {wind_turbine, 0}
                       ],

    Empty = ogonek_resources:empty(),
    Resources = Empty#resources{
                  iron_ore=10000,
                  gold=10000,
                  h2o=10000,
                  oil=10000,
                  planet=PlanetId,
                  updated=ogonek_util:now8601()},

    Build = fun({B, Level}) ->
                    case ogonek_buildings:get_definition(B) of
                        error ->
                            lager:error("user ~s - there is no building definition for ~p - skipping",
                                        [Planet#planet.owner, B]);
                        Def ->
                            ogonek_buildings:finish(Def, PlanetId, Level)
                    end
            end,
    lists:foreach(Build, InitialBuildings),

    Planet#planet{resources=Resources}.
