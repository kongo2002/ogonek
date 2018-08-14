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
          constructions :: [construction()]
         }).

-type planet_state() :: #planet_state{}.

-record(state, {
          id :: binary(),
          planets :: #{binary() => planet_state()},
          session :: pid()
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
    State = #state{id=UserId, planets=maps:new(), session=SessionDispatcher},

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
                                    PState = #planet_state{planet=P,
                                                           buildings=[],
                                                           constructions=[]},
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
                          Self ! {calc_resources, P, true}
                  end, maps:keys(PlanetMap)),

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

    % TODO

    json_to_sockets(ogonek_building, Building, State),
    {noreply, State};

handle_info({get_planets, Sender}, State) ->
    Planets = maps:keys(State#state.planets),
    Sender ! {planets, Planets},
    {noreply, State};

handle_info({calc_resources, PlanetId, Silent}, State) ->
    lager:debug("user ~s - calculating resources for ~s", [State#state.id, PlanetId]),

    case maps:get(PlanetId, State#state.planets, undefined) of
        undefined ->
            {noreply, State};
        PState ->
            Planet = PState#planet_state.planet,
            Buildings = PState#planet_state.buildings,

            % TODO: re-calculate power/workers only if necessary
            % that is if there are buildings that were finished since
            % the last time we calculated power/workers
            {Power, Workers} = ogonek_buildings:calculate_power_workers(Buildings),

            Res0 = calculate_resources(Planet, Buildings),
            Res1 = Res0#resources{power=Power, workers=Workers},

            ogonek_db:planet_update_resources(PlanetId, Res1),

            Planet0 = Planet#planet{resources=Res1},
            PState0 = PState#planet_state{planet=Planet0},
            Planets0 = maps:put(PlanetId, PState0, State#state.planets),
            State0 = State#state{planets=Planets0},

            if Silent == false ->
                   json_to_sockets(ogonek_resources, Res1, State0);
               true ->
                   ok
            end,

            {noreply, State0}
    end;

handle_info(planet_info, State) ->
    lists:foreach(fun(P) -> self() ! {planet_info, P} end,
                  maps:keys(State#state.planets)),
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
            Self ! {calc_resources, PlanetId, false}
    end,
    {noreply, State};

handle_info({build_building, Planet, Type, Level}=Req, State) ->
    case maps:get(Planet, State#state.planets, undefined) of
        undefined ->
            % TODO: respond with error?
            {noreply, State};
        #planet_state{buildings=Bs, constructions=Cs} ->
            case {get_building(Bs, Type), get_construction(Cs, Type)} of
                % no building of this available
                {undefined, _} ->
                    lager:info("user ~s - build-building rejected, building not available [request ~p]",
                               [State#state.id, Req]),
                    error;
                % already construction ongoing of this type
                {_, {ok, _Construction}} ->
                    lager:info("user ~s - build-building rejected, same construction ongoing [request ~p]",
                               [State#state.id, Req]),
                    error;
                % building available and no construction ongoing
                {{ok, Building}, undefined} ->
                    if Building#building.level + 1 == Level ->
                           % TODO: check and claim resources

                           Duration = ogonek_buildings:calculate_construction_duration(Type, Level),
                           FinishedAt = finished_at(Duration),

                           Construction = #construction{
                                             planet=Planet,
                                             building=Type,
                                             level=Level,
                                             created=ogonek_util:now8601(),
                                             finish=FinishedAt
                                            },
                           ogonek_db:construction_create(Construction),
                           ok;
                       true ->
                           lager:info("user ~s - build-building rejected [current ~p, request ~p]",
                                      [State#state.id, Building, Req]),
                           error
                    end
            end,
            {noreply, State}
    end;

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

            PState0 = PState#planet_state{buildings=Fetched},
            Planets0 = maps:put(Planet, PState0, State#state.planets),

            if Silent == false ->
                   json_to_sockets(ogonek_building, PState0#planet_state.buildings, State);
               true ->
                   ok
            end,

            {noreply, State#state{planets=Planets0}};
        % buildings already present
        PState ->
            if Silent == false ->
                   json_to_sockets(ogonek_building, PState#planet_state.buildings, State);
               true ->
                   ok
            end,
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

            % TODO: handle finished constructions

            PState0 = PState#planet_state{constructions=Fetched},
            Planets0 = maps:put(Planet, PState0, State#state.planets),

            if Silent == false ->
                   json_to_sockets(ogonek_construction, PState0#planet_state.constructions, State);
               true ->
                   ok
            end,

            {noreply, State#state{planets=Planets0}};
        % constructions already present
        PState ->
            if Silent == false ->
                   json_to_sockets(ogonek_construction, PState#planet_state.constructions, State);
               true ->
                   ok
            end,
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

fetch_planets(State) ->
    UserId = State#state.id,
    Planets = ogonek_planet_manager:user_planets(UserId),

    case Planets of
        [] ->
            lager:info("user ~s has no planets yet - assigning a free one now", [UserId]),
            {ok, Planet} = ogonek_planet_manager:claim_free_planet(UserId),
            bootstrap_free_planet(Planet),
            [Planet];
        Ps -> Ps
    end.


-spec bootstrap_free_planet(planet()) -> ok.
bootstrap_free_planet(Planet) ->
    lager:info("user ~s - bootstrapping initial infrastructure on planet ~s",
               [Planet#planet.owner, Planet#planet.id]),

    % this is a new free planet: let's populate
    % with initial set of infrastructure
    PlanetId = Planet#planet.id,
    InitialBuildings = [construction_center,
                        oil_rig,
                        water_rig,
                        ore_mine,
                        gold_mine,
                        oil_tank,
                        water_tank,
                        ore_depot,
                        gold_depot,
                        power_plant,
                        apartment_block],

    Build = fun(B) ->
                    case ogonek_buildings:get_definition(B) of
                        error ->
                            lager:error("user ~s - there is no building definition for ~p - skipping",
                                        [Planet#planet.owner, B]);
                        Def ->
                            finish_building(Def, PlanetId)
                    end
            end,
    lists:foreach(Build, InitialBuildings).


-spec seconds_since(resources()) -> integer().
seconds_since(Resources) ->
    Now = calendar:universal_time(),
    Since = iso8601:parse(Resources#resources.updated),
    calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(Since).


-spec seconds_to_simulated_hours(integer()) -> float().
seconds_to_simulated_hours(Seconds) ->
    % 1 hour simulated time == 30 minutes real-time
    Seconds / 1800.


-spec finished_at(integer()) -> timestamp().
finished_at(DurationSeconds) ->
    Now = calendar:universal_time(),
    GregorianNow = calendar:datetime_to_gregorian_seconds(Now),
    FinishedAt = GregorianNow + DurationSeconds,
    iso8601:format(calendar:gregorian_seconds_to_datetime(FinishedAt)).


-spec calculate_resources(planet(), [building()]) -> resources().
calculate_resources(Planet, Buildings) ->
    UserId = Planet#planet.owner,
    Resources = Planet#planet.resources,

    SecondsSince = seconds_since(Resources),

    if SecondsSince >= 60 ->
           % the production capabilities of the planet's buildings are the
           % base of the overall resource production
           % that productivity is calculated in relation to the planet's base resources
           Production = ogonek_buildings:calculate_building_production(Buildings),
           PlanetResources = ogonek_planet:production(Planet),
           CombinedProduction = ogonek_resources:multiply(Production, PlanetResources),
           lager:debug("user ~s - production: ~p", [UserId, CombinedProduction]),

           SimulatedHours = seconds_to_simulated_hours(SecondsSince),

           Produced = ogonek_resources:with_factor(SimulatedHours, CombinedProduction),
           lager:debug("user ~s - produced since ~s: ~p", [UserId, Resources#resources.updated, Produced]),

           Summed = ogonek_resources:sum(Resources, Produced),
           Summed#resources{updated=ogonek_util:now8601()};
       true ->
           lager:debug("user ~s - skipping resources calculation [~p sec ago]",
                       [UserId, SecondsSince]),
           Resources
    end.


-spec finish_building(bdef(), binary()) -> ok.
finish_building(Def, PlanetId) ->
    finish_building(Def, PlanetId, 1).


-spec finish_building(bdef(), binary(), integer()) -> ok.
finish_building(#bdef{name=Def}, PlanetId, Level) ->
    Now = ogonek_util:now8601(),
    Building = #building{planet=PlanetId,
                         type=Def,
                         level=Level,
                         created=Now},

    % maybe this one should be managed via the planet manager
    ogonek_db:building_finish(Building).


-spec json_to_sockets(atom(), term(), state()) -> ok.
json_to_sockets(Module, Obj, State) ->
    Session = State#state.session,
    gen_server:cast(Session, {json_to_sockets, Module, Obj}).


-spec get_building([building()], atom()) -> {ok, building()} | undefined.
get_building(Buildings, Type) ->
    case lists:keyfind(Type, 4, Buildings) of
        false -> undefined;
        Building -> {ok, Building}
    end.


-spec get_construction([construction()], atom()) -> {ok, construction()} | undefined.
get_construction(Constructions, Type) ->
    case lists:keyfind(Type, 3, Constructions) of
        false -> undefined;
        Construction -> {ok, Construction}
    end.
