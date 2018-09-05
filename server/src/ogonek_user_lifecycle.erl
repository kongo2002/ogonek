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

-include("ogonek.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

-record(planet_state, {
          planet :: planet(),
          buildings :: [building()],
          constructions :: [construction()],
          capacity :: capacity()
         }).

-define(OGONEK_REFRESH_RESOURCE_INTERVAL_SECS, 300).

-type planet_state() :: #planet_state{}.

-record(state, {
          id :: binary(),
          planets :: #{binary() => planet_state()},
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
    Planets = fetch_planets(State),
    PlanetMap = lists:foldl(fun(#planet{id=Id}=P, Ps) ->
                                    PState = #planet_state{
                                                planet=P,
                                                buildings=[],
                                                constructions=[],
                                                capacity=ogonek_capacity:empty(Id)},
                                    maps:put(Id, PState, Ps)
                            end, maps:new(), Planets),

    lager:debug("user '~s' has ~p planets: ~p",
                [UserId, maps:size(PlanetMap), maps:keys(PlanetMap)]),

    % trigger initialization of all planets
    lists:foreach(fun(P) ->
                          % get buildings of planet
                          Self ! {get_buildings, P, true},

                          % get open construction orders of planet
                          Self ! {get_constructions, P, true},

                          % calculate resources after that
                          Self ! {calc_resources, P, false},

                          % push production info as well
                          Self ! {production_info, P},

                          % fetch research progress
                          Self ! get_research
                  end, maps:keys(PlanetMap)),

    schedule_recalculate_resources(),

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
handle_info({building_finish, Building}, #state{id=Id}=State) ->
    lager:info("user ~s - building finished: ~p", [Id, Building]),

    PlanetId = Building#building.planet,

    case maps:get(PlanetId, State#state.planets, undefined) of
        undefined ->
            {noreply, State};
        PState ->
            BuildingType = Building#building.type,
            BuildingLevel = Building#building.level,
            Buildings = PState#planet_state.buildings,

            Buildings0 = update_building(Buildings, Building),
            Cs0 = remove_construction(PState#planet_state.constructions, BuildingType, BuildingLevel),

            % in case we finished a capacity-relevant building we
            % have to re-calculate the capacities
            Capacity = ogonek_capacity:from_buildings(PlanetId, Buildings0),

            % re-calculate the power/worker amounts in case a relevant
            % building was just finished
            Planet = PState#planet_state.planet,
            Res = Planet#planet.resources,
            {Power, Workers} = ogonek_buildings:calculate_power_workers(Buildings0, Cs0),
            Res1 = Res#resources{power=Power, workers=Workers},
            Planet0 = Planet#planet{resources=Res1},

            PState0 = PState#planet_state{
                        planet=Planet0,
                        buildings=Buildings0,
                        capacity=Capacity,
                        constructions=Cs0},

            Planets0 = maps:put(PlanetId, PState0, State#state.planets),
            State0 = State#state{planets=Planets0},

            ogonek_db:construction_remove(PlanetId, BuildingType, BuildingLevel),

            json_to_sockets(ogonek_building, Building, State0),
            json_to_sockets(ogonek_capacity, Capacity, State0),
            json_to_sockets(ogonek_resources, Res1, State0),

            {noreply, State0}
    end;

handle_info({construction_create, Construction}, #state{id=Id}=State) ->
    lager:info("user ~s - construction created: ~p", [Id, Construction]),

    % push construction to client
    json_to_sockets(ogonek_construction, Construction, State),

    % update resources as well
    PlanetId = Construction#construction.planet,
    case maps:get(PlanetId, State#state.planets, undefined) of
        undefined -> ok;
        #planet_state{planet=Planet} ->
            Resources = Planet#planet.resources,
            json_to_sockets(ogonek_resources, Resources, State)
    end,

    {noreply, State};

handle_info({get_planets, Sender}, State) ->
    Planets = maps:keys(State#state.planets),
    Sender ! {planets, Planets},
    {noreply, State};

handle_info(calc_resources, State) ->
    lists:foreach(fun(P) -> self() ! {calc_resources, P, false} end,
                  maps:keys(State#state.planets)),

    schedule_recalculate_resources(),
    {noreply, State};

handle_info({calc_resources, PlanetId, Silent}, State) ->
    lager:debug("user ~s - calculating resources for ~s", [State#state.id, PlanetId]),

    case maps:get(PlanetId, State#state.planets, undefined) of
        undefined ->
            {noreply, State};
        PState ->
            PState0 = calc_resources(PState),

            % update power/workers as well
            PState1 = update_power_workers(PState0),

            Planets0 = maps:put(PlanetId, PState1, State#state.planets),
            State0 = State#state{planets=Planets0},

            Planet = PState1#planet_state.planet,
            Res = Planet#planet.resources,

            json_to_sockets(ogonek_resources, Res, State0, Silent),

            {noreply, State0}
    end;

handle_info(planet_info, State) ->
    lists:foreach(fun(P) -> self() ! {planet_info, P} end,
                  maps:keys(State#state.planets)),

    self() ! get_research,

    {noreply, State};

handle_info(start_research, State) ->
    case current_research(State#state.research) of
        {undefined, Research} ->
            % TODO: proper calculation for research duration
            FinishedAt = finished_at(20 * 60),
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

            ogonek_db:research_create(Res),
            Rss = update_research(State#state.research, Res),

            trigger_research_check(Res),

            {noreply, State#state{research=Rss}};
        _Otherwise ->
            % research already ongoing
            {noreply, State}
    end;

handle_info({research_create, _Research}, State) ->
    self() ! get_research,
    {noreply, State};

handle_info({research_finish, _Research}, State) ->
    self() ! get_research,
    self() ! unlock_buildings,

    % reset research timer
    State0 = State#state{research_timer=undefined},

    {noreply, State0};

handle_info(unlock_buildings, State) ->
    UserId = State#state.id,
    Researches = State#state.research,

    lists:foreach(fun({PId, Planet}) ->
                          Buildings = Planet#planet_state.buildings,
                          case ogonek_buildings:unlocked_buildings(Buildings, Researches) of
                              [] -> ok;
                              Unlocked ->
                                  lager:info("user ~s - unlocked new buildings on planet ~s: ~p",
                                             [UserId, PId, Unlocked]),

                                  lists:foreach(fun(Def) ->
                                                        finish_building(Def, PId, 0)
                                                end, Unlocked)
                          end
                  end, maps:to_list(State#state.planets)),

    {noreply, State};

handle_info({planet_info, PlanetId}, State) ->
    case maps:get(PlanetId, State#state.planets, undefined) of
        undefined -> ok;
        PState ->
            % push planet at first
            json_to_sockets(ogonek_planet, PState#planet_state.planet, State),

            % trigger building and resource information after that
            Self = self(),
            Self ! {get_buildings, PlanetId, false},
            Self ! {get_constructions, PlanetId, false},
            Self ! {calc_resources, PlanetId, false},
            Self ! {production_info, PlanetId}
    end,
    {noreply, State};

handle_info({production_info, PlanetId}, State) ->
    case maps:get(PlanetId, State#state.planets, undefined) of
        undefined -> ok;
        #planet_state{planet=Planet, buildings=Buildings} ->
            Production = ogonek_production:of_planet(Planet, Buildings),
            WithConsumption = ogonek_buildings:apply_building_consumption(Production, Buildings),
            json_to_sockets(ogonek_production, WithConsumption, State)
    end,

    {noreply, State};

handle_info({build_building, Planet, Type, Level}=Req, State) ->
    case maps:get(Planet, State#state.planets, undefined) of
        undefined ->
            {noreply, State};
        #planet_state{buildings=Bs, constructions=Cs}=PState ->
            case {get_building(Bs, Type), get_construction(Cs, Type)} of
                % no building of this available
                {undefined, _} ->
                    lager:info("user ~s - build-building rejected, building not available [request ~p]",
                               [State#state.id, Req]),
                    {noreply, State};
                % already construction ongoing of this type
                {_, {ok, _Construction}} ->
                    lager:info("user ~s - build-building rejected, same construction ongoing [request ~p]",
                               [State#state.id, Req]),
                    {noreply, State};
                % building available and no construction ongoing
                {{ok, Building}, undefined} ->
                    if Building#building.level + 1 == Level ->
                           Costs = ogonek_buildings:calculate_building_costs(Building),
                           Possible = construction_possible(PState, Costs),

                           if Possible == true ->
                                  Duration = ogonek_buildings:calculate_construction_duration(Building),
                                  FinishedAt = finished_at(Duration),

                                  Construction = #construction{
                                                    planet=Planet,
                                                    building=Type,
                                                    level=Level,
                                                    created=ogonek_util:now8601(),
                                                    finish=FinishedAt
                                                   },
                                  ogonek_db:construction_create(Construction),

                                  % we keep the new construction in state already although
                                  % the database store might still be ongoing
                                  % however we want to prevent multiple successive constructions
                                  % to be happening
                                  Cs0 = update_construction(Cs, Construction),
                                  PState0 = PState#planet_state{constructions=Cs0},
                                  PState1 = claim_resources(PState0, Costs),
                                  Planets0 = maps:put(Planet, PState1, State#state.planets),

                                  % schedule check for the time the construction will be finished
                                  trigger_construction_check(Planet, Duration + 1),

                                  State0 = State#state{planets=Planets0},
                                  {noreply, State0};
                              true ->
                                  lager:info("user ~s - build-building rejected due to insufficient resources [costs ~p, request ~p]",
                                             [State#state.id, Costs, Req]),
                                  {noreply, State}
                           end;
                       true ->
                           lager:info("user ~s - build-building rejected [current ~p, request ~p]",
                                      [State#state.id, Building, Req]),
                           {noreply, State}
                    end
            end
    end;

handle_info({get_utilization, PlanetId}, State) ->
    case maps:get(PlanetId, State#state.planets, undefined) of
        % unknown, invalid or foreign planet
        undefined -> ok;
        % buildings not fetched yet
        #planet_state{planet=Planet} ->
            Utilization = Planet#planet.utilization,
            json_to_sockets(ogonek_utilization, Utilization, State)
    end,
    {noreply, State};

handle_info({set_utilization, PlanetId, Resource, Value}, State) ->
    case maps:get(PlanetId, State#state.planets, undefined) of
        % unknown, invalid or foreign planet
        undefined ->
            {noreply, State};
        % buildings not fetched yet
        #planet_state{planet=Planet}=PState ->
            Utilization = Planet#planet.utilization,

            case ogonek_utilization:validate(Utilization, Resource, Value) of
                {ok, Updated} ->
                    lager:info("user ~s - updating utilization to ~p", [State#state.id, Updated]),

                    Planet0 = Planet#planet{utilization=Updated},
                    PState0 = PState#planet_state{planet=Planet0},
                    Planets = maps:put(PlanetId, PState0, State#state.planets),

                    ogonek_db:planet_update_utilization(PlanetId, Updated),

                    {noreply, State#state{planets=Planets}};
                error ->
                    {noreply, State}
            end
    end;

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

            ogonek_db:research_finish(Pending1),

            Updated = update_research(Finished0, Pending1),
            {undefined, Updated, Updated};
        {P, F} ->
            % there is a pending research that is not finished yet
            {P, F, Research}
    end,

    % create new research completion timer if not set yet
    State0 =
    case {Pending, State#state.research_timer} of
        {#research{}, undefined} ->
            State#state{research_timer=trigger_research_check(Pending)};
        _Otherwise ->
            State
    end,

    Json = ogonek_research:research_info_json(Pending, Finished),
    gen_server:cast(State0#state.session, {json_to_sockets, Json}),

    State1 = State0#state{research=Research0},

    {noreply, State1};

handle_info({get_buildings, Planet, Silent}, State) ->
    case maps:get(Planet, State#state.planets, undefined) of
        % unknown, invalid or foreign planet
        undefined ->
            {noreply, State};
        % buildings not fetched yet
        #planet_state{buildings=[]}=PState ->
            Fetched = ogonek_db:buildings_of_planet(Planet),

            lager:debug("user ~s - fetched buildings of planet ~s: ~p",
                        [State#state.id, Planet, Fetched]),

            Capacity = ogonek_capacity:from_buildings(Planet, Fetched),
            PState0 = PState#planet_state{buildings=Fetched, capacity=Capacity},

            Planets0 = maps:put(Planet, PState0, State#state.planets),

            json_to_sockets(ogonek_building, PState0#planet_state.buildings, State, Silent),
            json_to_sockets(ogonek_capacity, Capacity, State, Silent),

            {noreply, State#state{planets=Planets0}};
        % buildings already present
        PState ->
            json_to_sockets(ogonek_building, PState#planet_state.buildings, State, Silent),
            json_to_sockets(ogonek_capacity, PState#planet_state.capacity, State, Silent),
            {noreply, State}
    end;

handle_info({get_constructions, Planet, Silent}, State) ->
    case maps:get(Planet, State#state.planets, undefined) of
        % unknown, invalid or foreign planet
        undefined ->
            {noreply, State};
        % constructions not fetched yet
        #planet_state{constructions=[]}=PState ->
            Fetched = ogonek_db:constructions_of_planet(Planet),

            lager:debug("user ~s - fetched constructions of planet ~s: ~p",
                        [State#state.id, Planet, Fetched]),

            PState0 = process_constructions(PState, Fetched),

            trigger_construction_checks(PState0#planet_state.constructions),

            Planets0 = maps:put(Planet, PState0, State#state.planets),

            json_to_sockets(ogonek_construction, PState0#planet_state.constructions, State, Silent),

            {noreply, State#state{planets=Planets0}};
        % constructions already present
        PState ->
            json_to_sockets(ogonek_construction, PState#planet_state.constructions, State, Silent),
            {noreply, State}
    end;

handle_info({process_constructions, PlanetId}, #state{id=Id}=State) ->
    case maps:get(PlanetId, State#state.planets, undefined) of
        % unknown, invalid or foreign planet
        undefined ->
            {noreply, State};
        #planet_state{constructions=Cs}=PState ->
            lager:debug("user ~s - processing constructions", [Id]),

            PState0 = process_constructions(PState, Cs),
            Planets0 = maps:put(PlanetId, PState0, State#state.planets),

            {noreply, State#state{planets=Planets0}}
    end;

handle_info({planet_claim, Planet}, State) ->
    PlanetId = Planet#planet.id,
    Planets = State#state.planets,
    Planet0 = bootstrap_free_planet(Planet),
    PState = #planet_state{
                planet=Planet0,
                buildings=[],
                constructions=[],
                capacity=ogonek_capacity:empty(PlanetId)},
    Planets0 = maps:put(PlanetId, PState, Planets),

    % update resources in db
    ogonek_db:planet_update_resources(PlanetId, Planet0#planet.resources),

    self() ! {planet_info, PlanetId},

    {noreply, State#state{planets=Planets0}};

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
terminate(_Reason, _State) ->
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
        [] -> ogonek_db:research_of_user(State#state.id);
        Research -> Research
    end.


-spec update_power_workers(planet_state()) -> planet_state().
update_power_workers(PState) ->
    Planet = PState#planet_state.planet,
    Buildings = PState#planet_state.buildings,
    Constructions = PState#planet_state.constructions,

    {Power, Workers} = ogonek_buildings:calculate_power_workers(Buildings, Constructions),

    Res = Planet#planet.resources,
    Res1 = Res#resources{power=Power, workers=Workers},
    Planet0 = Planet#planet{resources=Res1},
    PState#planet_state{planet=Planet0}.


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
                            finish_building(Def, PlanetId, Level)
                    end
            end,
    lists:foreach(Build, InitialBuildings),

    Planet#planet{resources=Resources}.


-spec schedule_recalculate_resources() -> ok.
schedule_recalculate_resources() ->
    Interval = ?OGONEK_REFRESH_RESOURCE_INTERVAL_SECS * 1000,
    erlang:send_after(Interval, self(), calc_resources),
    ok.


-spec seconds_since(Timestamp :: binary()) -> integer().
seconds_since(Timestamp) ->
    seconds_since(Timestamp, ogonek_util:now8601()).


-spec seconds_since(Timestamp :: timestamp(), RelativeTo :: timestamp()) -> integer().
seconds_since(Timestamp, RelativeTo) ->
    RelativeTime = iso8601:parse(RelativeTo),
    Since = iso8601:parse(Timestamp),
    abs(calendar:datetime_to_gregorian_seconds(RelativeTime) - calendar:datetime_to_gregorian_seconds(Since)).


-spec seconds_to_simulated_hours(integer()) -> float().
seconds_to_simulated_hours(Seconds) ->
    % 1 hour simulated time == 30 minutes real-time
    Seconds / 1800.


-spec finished_at(Seconds :: integer()) -> timestamp().
finished_at(DurationSeconds) ->
    Now = calendar:universal_time(),
    GregorianNow = calendar:datetime_to_gregorian_seconds(Now),
    FinishedAt = GregorianNow + DurationSeconds,
    iso8601:format(calendar:gregorian_seconds_to_datetime(FinishedAt)).


-spec calc_resources(planet_state()) -> planet_state().
calc_resources(PState) ->
    calc_resources(PState, ogonek_util:now8601()).


-spec calc_resources(planet_state(), timestamp()) -> planet_state().
calc_resources(PState, RelativeTo) ->
    Planet = PState#planet_state.planet,
    Buildings = PState#planet_state.buildings,

    case calculate_resources(PState, Buildings, RelativeTo) of
        skipped ->
            PState;
        Resources ->
            ogonek_db:planet_update_resources(Planet#planet.id, Resources),

            Planet0 = Planet#planet{resources=Resources},
            PState#planet_state{planet=Planet0}
    end.


-spec calculate_resources(planet_state(), [building()], timestamp()) -> resources() | skipped.
calculate_resources(PlanetState, Buildings, RelativeTo) ->
    calculate_resources(PlanetState, Buildings, RelativeTo, false).


-spec calculate_resources(planet_state(), [building()], timestamp(), boolean()) -> resources() | skipped.
calculate_resources(PlanetState, Buildings, RelativeTo, Force) ->
    Planet = PlanetState#planet_state.planet,
    UserId = Planet#planet.owner,
    Resources = Planet#planet.resources,
    Capacity = PlanetState#planet_state.capacity,

    SecondsSince = seconds_since(Resources#resources.updated, RelativeTo),

    if SecondsSince >= 60 orelse Force == true ->
           Production = ogonek_production:of_planet(Planet, Buildings),
           lager:debug("user ~s - production: ~p", [UserId, Production]),

           SimulatedHours = seconds_to_simulated_hours(SecondsSince),

           Produced = ogonek_resources:with_factor(SimulatedHours, Production),
           lager:debug("user ~s - produced since ~s: ~p", [UserId, Resources#resources.updated, Produced]),

           Summed = ogonek_resources:sum(Resources, Produced),
           AfterConsumption = ogonek_buildings:calculate_building_consumption(Summed, Buildings, SimulatedHours),

           lager:debug("user ~s - after consumption: ~p", [UserId, AfterConsumption]),

           Capped = ogonek_resources:with_capacity(AfterConsumption, Capacity),
           Capped#resources{updated=RelativeTo};
       true ->
           lager:debug("user ~s - skipping resources calculation [~p sec ago]",
                       [UserId, SecondsSince]),
           skipped
    end.


-spec finish_building(bdef(), binary(), integer()) -> ok.
finish_building(#bdef{name=Def}, PlanetId, Level) ->
    Now = ogonek_util:now8601(),
    Building = #building{planet=PlanetId,
                         type=Def,
                         level=Level,
                         created=Now},

    % maybe this one should be managed via the planet manager
    ogonek_db:building_finish(Building).


-spec process_constructions(planet_state(), [construction()]) -> planet_state().
process_constructions(PState, []) -> PState;
process_constructions(PState, Constructions) ->
    {Finished, Running} = split_constructions(Constructions),

    PState0 = lists:foldl(fun process_construction/2, PState, Finished),
    PState0#planet_state{constructions=Running}.


-spec process_construction({construction(), timestamp()}, planet_state()) -> planet_state().
process_construction({Construction, UpTo}, PState) ->
    Planet = PState#planet_state.planet,
    Buildings = PState#planet_state.buildings,
    PlanetId = Planet#planet.id,
    Type = Construction#construction.building,

    case get_building(Buildings, Type) of
        {ok, B} ->
            CLevel = Construction#construction.level,
            BLevel = B#building.level,
            if BLevel + 1 == CLevel ->
                   Update = B#building{level=CLevel},

                   % trigger asynchronous db update
                   ogonek_db:building_finish(Update),

                   % in order to properly calculate multiple successive
                   % constructions we have to re-calculate resources
                   % in here already
                   Buildings0 = update_building(Buildings, Update),
                   Capacity = ogonek_capacity:from_buildings(PlanetId, Buildings0),

                   PState0 = PState#planet_state{
                              buildings=Buildings0,
                              capacity=Capacity},

                   calc_resources(PState0, UpTo);
               true ->
                   lager:warning("invalid construction building level: ~p",
                                 [Construction]),
                   PState
            end;
        undefined ->
            lager:warning("construction finished for unknown building: ~p",
                          [Construction]),
            PState
    end.


-spec split_constructions([construction()]) ->
    {[{construction(), timestamp()}], [construction()]}.
split_constructions(Constructions) ->
    split_constructions(Constructions, ogonek_util:now8601()).


-spec split_constructions([construction()], RelativeTo :: timestamp()) ->
    {[{construction(), timestamp()}], [construction()]}.
split_constructions(Constructions, RelativeTo) ->
    % divide the list of constructions into finished and running ones
    {Finished, StillRunning} = lists:partition(fun(#construction{finish=F}) ->
                                                       F =< RelativeTo
                                               end, Constructions),

    % after that we want to prepare the finished constructions
    % by sorting by finish timestamp as well
    SortedFinished = lists:keysort(7, Finished),

    % now we add the respective next timestamp to which the
    % resources have to be re-calculated until
    Ts = lists:map(fun(#construction{finish=F}) -> F end, SortedFinished),
    % the last construction will be calculated up to 'now'
    Ts0 = tl(Ts ++ [RelativeTo]),

    {lists:zip(SortedFinished, Ts0), StillRunning}.


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


-spec json_to_sockets(atom(), term(), state()) -> ok.
json_to_sockets(Module, Obj, State) ->
    Session = State#state.session,
    gen_server:cast(Session, {json_to_sockets, Module, Obj}).


-spec json_to_sockets(atom(), term(), state(), boolean()) -> ok.
json_to_sockets(_Module, _Obj, _State, true) -> ok;
json_to_sockets(Module, Obj, State, _) ->
    json_to_sockets(Module, Obj, State).


-spec trigger_research_check(research()) -> reference().
trigger_research_check(Research) ->
    DueIn = seconds_since(Research#research.finish) + 1,
    erlang:send_after(DueIn * 1000, self(), get_research).


-spec trigger_construction_checks([construction()]) -> ok.
trigger_construction_checks(Constructions) ->
    lists:foreach(fun(C) ->
                          Planet = C#construction.planet,
                          DueIn = seconds_since(C#construction.finish),
                          trigger_construction_check(Planet, DueIn + 1)
                  end, Constructions).


-spec trigger_construction_check(PlanetId :: binary(), Seconds :: integer()) -> ok.
trigger_construction_check(PlanetId, Seconds) ->
    erlang:send_after(Seconds * 1000, self(), {process_constructions, PlanetId}),
    ok.


-spec get_building([building()], atom()) -> {ok, building()} | undefined.
get_building(Buildings, Type) ->
    case lists:keyfind(Type, 4, Buildings) of
        false -> undefined;
        Building -> {ok, Building}
    end.


-spec update_building([building()], building()) -> [building()].
update_building(Buildings, #building{type=Type}=Building) ->
    case get_building(Buildings, Type) of
        undefined -> [Building | Buildings];
        _Otherwise ->
            lists:keyreplace(Type, 4, Buildings, Building)
    end.


-spec get_construction([construction()], atom()) -> {ok, construction()} | undefined.
get_construction(Constructions, Type) ->
    case lists:keyfind(Type, 3, Constructions) of
        false -> undefined;
        Construction -> {ok, Construction}
    end.


-spec update_construction([construction()], construction()) -> [construction()].
update_construction(Constructions, #construction{building=Type}=Construction) ->
    case get_construction(Constructions, Type) of
        undefined -> [Construction | Constructions];
        _Otherwise ->
            lists:keyreplace(Type, 3, Constructions, Construction)
    end.


-spec remove_construction([construction()], atom(), integer()) -> [construction()].
remove_construction(Constructions, Type, Level) ->
    lists:foldl(fun(#construction{building=T, level=Lvl}, Cs)
                      when T == Type andalso Lvl =< Level -> Cs;
                   (C, Cs) -> [C | Cs]
                end, [], Constructions).


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


-spec construction_possible(PlanetState :: planet_state(), Costs :: bdef()) -> boolean().
construction_possible(PlanetState, Costs) ->
    Planet = PlanetState#planet_state.planet,
    Constructions = PlanetState#planet_state.constructions,
    Buildings = PlanetState#planet_state.buildings,
    Resources = Planet#planet.resources,

    NumConstructions = length(Constructions),
    MaxConcurrentConstructions = max_concurrent_constructions(Buildings),

    MaxConcurrentConstructions > NumConstructions andalso

    Resources#resources.workers >= Costs#bdef.workers andalso
    Resources#resources.power >= Costs#bdef.power andalso

    Resources#resources.iron_ore >= Costs#bdef.iron_ore andalso
    Resources#resources.gold >= Costs#bdef.gold andalso
    Resources#resources.h2o >= Costs#bdef.h2o andalso
    Resources#resources.oil >= Costs#bdef.oil andalso
    Resources#resources.h2 >= Costs#bdef.h2 andalso
    Resources#resources.uranium >= Costs#bdef.uranium andalso
    Resources#resources.pvc >= Costs#bdef.pvc andalso
    Resources#resources.titan >= Costs#bdef.titan andalso
    Resources#resources.kyanite >= Costs#bdef.kyanite.


-spec max_concurrent_constructions([building()]) -> integer().
max_concurrent_constructions(Buildings) ->
    CCLevel = construction_center_level(Buildings),
    CCLevel div 10 + 1.


-spec construction_center_level([building()]) -> integer().
construction_center_level(Buildings) ->
    case get_building(Buildings, construction_center) of
        {ok, #building{level=Level}} -> Level;
        _Otherwise -> 0
    end.


-spec claim_resources(PlanetState :: planet_state(), Costs :: bdef()) -> planet_state().
claim_resources(PlanetState, Costs) ->
    Planet = PlanetState#planet_state.planet,
    Resources = Planet#planet.resources,

    % claim_resources is called immediately on the creation of the construction
    % that's why we claim only the 'positive' costs at this point
    % see 'ogonek_resources:substract_costs'
    Res0 = ogonek_resources:substract_costs(Resources, Costs),

    Planet0 = Planet#planet{resources=Res0},
    PlanetState#planet_state{planet=Planet0}.


%%
%% TESTS
%%

-ifdef(TEST).

update_building_test_() ->
    P = <<"p1">>,
    Now = ogonek_util:now8601(),
    B1 = #building{type=gold_depot, level=1, planet=P, created=Now},
    B2 = #building{type=gold_depot, level=2, planet=P, created=Now},

    [?_assertEqual([B1], update_building([], B1)),
     ?_assertEqual([B1], update_building([B1], B1)),
     ?_assertEqual([B2], update_building([B1], B2))
    ].

update_construction_test_() ->
    P = <<"p1">>,
    Now = ogonek_util:now8601(),
    C1 = #construction{building=gold_depot, level=1, planet=P, created=Now, finish=Now},
    C2 = #construction{building=gold_depot, level=2, planet=P, created=Now, finish=Now},

    [?_assertEqual([C1], update_construction([], C1)),
     ?_assertEqual([C1], update_construction([C1], C1)),
     ?_assertEqual([C2], update_construction([C1], C2))
    ].

remove_construction_test_() ->
    P = <<"p1">>,
    Now = ogonek_util:now8601(),
    C1 = #construction{building=gold_depot, level=1, planet=P, created=Now, finish=Now},
    C2 = #construction{building=gold_depot, level=2, planet=P, created=Now, finish=Now},

    [?_assertEqual([], remove_construction([], gold_depot, 1)),
     ?_assertEqual([C1], remove_construction([C1], oil_depot, 1)),
     ?_assertEqual([C2], remove_construction([C2], gold_depot, 1)),
     ?_assertEqual([], remove_construction([C2], gold_depot, 2))
    ].

split_constructions_test_() ->
    P = <<"p1">>,
    Past = <<"2000-08-19T06:49:04Z">>,
    Between = <<"2010-08-09T16:49:04Z">>,
    Now = ogonek_util:now8601(),
    C1 = #construction{building=gold_depot, level=1, planet=P, created=Past, finish=Now},
    C2 = #construction{building=gold_depot, level=2, planet=P, created=Past, finish=Past},
    C3 = #construction{building=gold_depot, level=3, planet=P, created=Past, finish=Between},

    [?_assertEqual({[], []}, split_constructions([])),
     ?_assertEqual({[{C2, Between}], [C1]}, split_constructions([C1, C2], Between)),
     ?_assertEqual({[{C2, Between}], [C1]}, split_constructions([C2, C1], Between)),
     ?_assertEqual({[{C2, Between}, {C3, Now}], []}, split_constructions([C2, C3], Now)),
     ?_assertEqual({[{C2, Between}, {C3, Now}], []}, split_constructions([C3, C2], Now))
    ].

-endif.
