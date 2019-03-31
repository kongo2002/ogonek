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

-module(ogonek_user_planet).

-include("include/ogonek.hrl").

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


-define(OGONEK_REFRESH_RESOURCE_INTERVAL_SECS, 300).

-type weapon_map() :: #{atom() => weapon()}.

-type ship_map() :: #{atom() => ship()}.


-record(state, {
          user :: binary(),
          planet :: planet(),
          session :: pid(),
          buildings :: [building()],
          constructions :: [construction()],
          capacity :: capacity(),
          weapon_orders :: [weapon_order()],
          weapons :: weapon_map(),
          ship_orders :: [ship_order()],
          ships :: ship_map()
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
start_link(Planet, Session) ->
    gen_server:start_link(?MODULE, {Planet, Session}, []).

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
init({Planet, Session}) ->
    Id = Planet#planet.id,
    State = #state{
               user=Planet#planet.owner,
               planet=Planet,
               session=Session,
               buildings=[],
               constructions=[],
               capacity=ogonek_capacity:empty(Id),
               weapon_orders=[],
               weapons=maps:new(),
               ship_orders=[],
               ships=maps:new()
              },

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
handle_call(get_buildings, _From, State) ->
    {reply, State#state.buildings, State};

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
handle_cast(prepare, State) ->
    Self = self(),
    PlanetId = planet_id(State),

    lager:info("planet ~s - starting preparation", [PlanetId]),

    % get buildings of planet
    Self ! {get_buildings, true},

    % get open construction orders of planet
    Self ! {get_constructions, true},

    % fetch open weapon orders
    % and weapons just before that
    Self ! {weapons_info, true},
    Self ! {get_weapon_orders, true},

    % fetch open ship orders
    % and ships just before that
    Self ! {ships_info, true},
    Self ! {get_ship_orders, true},

    % calculate resources after that
    Self ! {calc_resources, false},

    % push production info as well
    Self ! production_info,

    schedule_recalculate_resources(),

    {noreply, State};

handle_cast({terminate, Reason}, State) ->
    PlanetId = planet_id(State),
    UserId = user_id(State),

    lager:debug("user ~s - terminating planet ~s due to ~p",
                [UserId, PlanetId, Reason]),

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
handle_info({building_finish, Building}, State) ->
    UserId = user_id(State),
    PlanetId = Building#building.planet,
    BuildingType = Building#building.type,
    BuildingLevel = Building#building.level,
    Buildings = State#state.buildings,

    lager:info("user ~s - building finished: ~p", [UserId, Building]),

    Buildings0 = update_building(Buildings, Building),
    Cs0 = remove_construction(State#state.constructions, BuildingType, BuildingLevel),

    % in case we finished a capacity-relevant building we
    % have to re-calculate the capacities
    Capacity = ogonek_capacity:from_buildings(PlanetId, Buildings0),

    % re-calculate the power/worker amounts in case a relevant
    % building was just finished
    Planet = State#state.planet,
    Res = Planet#planet.resources,
    {Power, Workers} = ogonek_buildings:calculate_power_workers(Buildings0, Cs0),
    Res1 = Res#resources{power=Power, workers=Workers},
    Planet0 = Planet#planet{resources=Res1},

    State0 = State#state{
               planet=Planet0,
               buildings=Buildings0,
               capacity=Capacity,
               constructions=Cs0},

    ogonek_mongo:construction_remove(PlanetId, BuildingType, BuildingLevel),

    json_to_sockets(ogonek_building, Building, State0),
    json_to_sockets(ogonek_capacity, Capacity, State0),
    json_to_sockets(ogonek_resources, Res1, State0),

    % push production info
    self() ! production_info,

    {noreply, State0};

handle_info({construction_create, Construction}, State) ->
    UserId = user_id(State),

    lager:info("user ~s - construction created: ~p", [UserId, Construction]),

    % push construction to client
    json_to_sockets(ogonek_construction, Construction, State),

    % update resources as well
    Resources = resources(State),
    json_to_sockets(ogonek_resources, Resources, State),

    {noreply, State};

handle_info({weapon_order_create, WOrder}, State) ->
    UserId = user_id(State),

    lager:info("user ~s - weapon order created: ~p", [UserId, WOrder]),

    % push construction to client
    json_to_sockets(ogonek_weapon_order, WOrder, State),

    % publish resource update
    Resources = resources(State),
    json_to_sockets(ogonek_resources, Resources, State),

    trigger_weapon_order_checks([WOrder]),

    % update weapon orders
    Orders = [WOrder | State#state.weapon_orders],
    State0 = State#state{weapon_orders=Orders},

    {noreply, State0};

handle_info({unlock_buildings, Researches}, State) ->
    UserId = user_id(State),
    Planet = State#state.planet,
    Buildings = State#state.buildings,

    case ogonek_buildings:unlocked_buildings(Buildings, Researches) of
        [] -> ok;
        Unlocked ->
            lager:info("user ~s - unlocked new buildings on planet ~s: ~p",
                       [UserId, Planet#planet.id, Unlocked]),

            lists:foreach(fun(Def) ->
                                  ogonek_buildings:finish(Def, Planet#planet.id, 0)
                          end, Unlocked)
    end,
    {noreply, State};

handle_info({build_building, Research, Type, Level}=Req, State) ->
    Bs = State#state.buildings,
    Cs = State#state.constructions,
    Planet = State#state.planet,
    PlanetId = Planet#planet.id,

    case {ogonek_buildings:get_building(Bs, Type), get_construction(Cs, Type)} of
        % no building of this available
        {undefined, _} ->
            lager:info("user ~s - build-building rejected, building not available [request ~p]",
                       [State#state.user, Req]),
            {noreply, State};
        % already construction ongoing of this type
        {_, {ok, _Construction}} ->
            lager:info("user ~s - build-building rejected, same construction ongoing [request ~p]",
                       [State#state.user, Req]),
            {noreply, State};
        % building available and no construction ongoing
        {{ok, Building}, undefined} ->
            if Building#building.level + 1 == Level ->
                   Costs = ogonek_buildings:calculate_building_costs(Building),
                   Possible = construction_possible(State, Research, Costs),

                   if Possible == true ->
                          Duration = ogonek_buildings:calculate_construction_duration(Building),
                          FinishedAt = ogonek_util:finished_at(Duration),

                          Construction = #construction{
                                            planet=PlanetId,
                                            building=Type,
                                            level=Level,
                                            created=ogonek_util:now8601(),
                                            finish=FinishedAt
                                           },
                          ogonek_mongo:construction_create(Construction),

                          % we keep the new construction in state already although
                          % the database store might still be ongoing
                          % however we want to prevent multiple successive constructions
                          % to be happening
                          Cs0 = update_construction(Cs, Construction),
                          State0 = State#state{constructions=Cs0},
                          State1 = claim_resources(State0, Costs),

                          % schedule check for the time the construction will be finished
                          trigger_construction_check(Duration + 1),

                          {noreply, State1};
                      true ->
                          lager:info("user ~s - build-building rejected due to insufficient resources [costs ~p, request ~p]",
                                     [State#state.user, Costs, Req]),
                          {noreply, State}
                   end;
               true ->
                   lager:info("user ~s - build-building rejected [current ~p, request ~p]",
                              [State#state.user, Building, Req]),
                   {noreply, State}
            end
    end;

handle_info({weapon_update, Weapon, OrderId}, State) ->
    UserId = user_id(State),

    lager:info("user ~s - weapon updated: ~p [order ~s]", [UserId, Weapon, OrderId]),

    % delete associated weapon order
    ogonek_mongo:weapon_order_remove(OrderId),

    PlanetId = Weapon#weapon.planet,
    State0 = remove_weapon_order(State, PlanetId, OrderId),

    OrderFinished = ogonek_util:doc(<<"w_order_finished">>,
                                    [{<<"_id">>, OrderId}, {<<"planet">>, PlanetId}]),
    json_to_sockets(OrderFinished, State0),

    self() ! {weapons_info, false},

    {noreply, State0};

handle_info({ship_update, Ship, OrderId}, State) ->
    UserId = user_id(State),

    lager:info("user ~s - ship updated: ~p [order ~s]", [UserId, Ship, OrderId]),

    % delete associated ship order
    ogonek_mongo:ship_order_remove(OrderId),

    PlanetId = Ship#ship.planet,
    State0 = remove_ship_order(State, PlanetId, OrderId),

    OrderFinished = ogonek_util:doc(<<"s_order_finished">>,
                                    [{<<"_id">>, OrderId}, {<<"planet">>, PlanetId}]),
    json_to_sockets(OrderFinished, State0),

    self() ! {ships_info, false},

    {noreply, State0};

handle_info(schedule_calc_resources, State) ->
    self() ! {calc_resources, false},
    schedule_recalculate_resources(),

    {noreply, State};

handle_info({calc_resources, Silent}, State) ->
    PlanetId = planet_id(State),
    UserId = user_id(State),

    lager:debug("user ~s - calculating resources for ~s", [UserId, PlanetId]),

    State0 = calc_resources(State),

    % update power/workers as well
    State1 = update_power_workers(State0),
    Res = resources(State1),

    json_to_sockets(ogonek_resources, Res, State1, Silent),

    {noreply, State1};

handle_info({weapons_info, Silent}, State) ->
    Buildings = State#state.buildings,

    case has_weapon_manufacture(Buildings) of
        true ->
            UserId = user_id(State),
            PlanetId = planet_id(State),
            Weapons = State#state.weapons,

            Weapons0 =
            case maps:size(Weapons) =< 0 of
                true ->
                    % fetch weapons from database
                    Fetched = fetch_weapons(PlanetId),
                    lager:info("user ~s - fetched weapons: ~p",
                               [UserId, Fetched]),
                    Fetched;
                false ->
                    Weapons
            end,

            Info = weapons_info(PlanetId, Weapons0),
            json_to_sockets(ogonek_weapon, Info, State, Silent),

            State0 = State#state{weapons=Weapons0},
            {noreply, State0};
        false ->
            {noreply, State}
    end;

handle_info({ships_info, Silent}, State) ->
    Buildings = State#state.buildings,

    case has_space_shipyard(Buildings) of
        true ->
            UserId = user_id(State),
            PlanetId = planet_id(State),
            Ships = State#state.ships,

            Ships0 =
            case maps:size(Ships) =< 0 of
                true ->
                    % fetch ships from database
                    Fetched = fetch_ships(PlanetId),
                    lager:info("user ~s - fetched ships: ~p",
                               [UserId, Fetched]),
                    Fetched;
                false ->
                    Ships
            end,

            Info = ships_info(PlanetId, Ships0),
            json_to_sockets(ogonek_ship, Info, State, Silent),

            State0 = State#state{ships=Ships0},
            {noreply, State0};
        false ->
            {noreply, State}

    end;

handle_info(planet_info, State) ->
    % push planet at first
    json_to_sockets(ogonek_planet, State#state.planet, State),

    % trigger building and resource information after that
    Self = self(),
    Self ! {get_buildings, false},
    Self ! {get_constructions, false},
    Self ! {get_weapon_orders, false},
    Self ! {weapons_info, false},
    Self ! {calc_resources, false},
    Self ! production_info,

    {noreply, State};

handle_info(production_info, State) ->
    Planet = State#state.planet,
    Buildings = State#state.buildings,

    Production = ogonek_production:of_planet(Planet, Buildings),
    Utilization = Planet#planet.utilization,
    WithConsumption = ogonek_buildings:apply_building_consumption(Production, Utilization, Buildings),
    json_to_sockets(ogonek_production, WithConsumption, State),

    {noreply, State};

handle_info({build_weapon, WDef}, State) ->
    PlanetId = planet_id(State),
    UserId = user_id(State),

    case weapon_order_possible(State, WDef) of
        true ->
            Bs = State#state.buildings,
            Duration = ogonek_weapons:calculate_order_duration(Bs, WDef),
            FinishedAt = ogonek_util:finished_at(Duration),
            Order = #weapon_order{
                       weapon=WDef#wdef.name,
                       planet=PlanetId,
                       created=ogonek_util:now8601(),
                       finish=FinishedAt
                      },

            lager:info("user ~s - start weapon order ~p [~s]", [UserId, Order, PlanetId]),

            ogonek_mongo:weapon_order_create(Order),

            State0 = claim_resources(State, WDef),
            {noreply, State0};
        false ->
            lager:warning("user ~s - build weapon not possible ~p [~s]", [UserId, WDef, PlanetId]),
            {noreply, State}
    end;

handle_info({build_ship, SDef}, State) ->
    PlanetId = planet_id(State),
    UserId = user_id(State),

    case ship_order_possible(State, SDef) of
        true ->
            Bs = State#state.buildings,
            Duration = ogonek_ships:calculate_order_duration(Bs, SDef),
            FinishedAt = ogonek_util:finished_at(Duration),
            Order = #ship_order{
                       ship=SDef#sdef.name,
                       planet=PlanetId,
                       created=ogonek_util:now8601(),
                       finish=FinishedAt
                      },

            lager:info("user ~s - start ship order ~p [~s]", [UserId, Order, PlanetId]),

            ogonek_mongo:ship_order_create(Order),

            State0 = claim_resources(State, SDef),
            {noreply, State0};
        false ->
            lager:warning("user ~s - build ship not possible ~p [~s]", [UserId, SDef, PlanetId]),
            {noreply, State}
    end;

handle_info(get_utilization, State) ->
    Planet = State#state.planet,
    Utilization = Planet#planet.utilization,

    json_to_sockets(ogonek_utilization, Utilization, State),

    {noreply, State};

handle_info({set_utilization, Resource, Value}, State) ->
    Planet = State#state.planet,
    Utilization = Planet#planet.utilization,

    case ogonek_utilization:validate(Utilization, Resource, Value) of
        {ok, Updated} ->
            UserId = user_id(State),
            PlanetId = planet_id(State),

            lager:info("user ~s - updating utilization to ~p [~s]",
                       [UserId, Updated, PlanetId]),

            Planet0 = Planet#planet{utilization=Updated},
            State0 = State#state{planet=Planet0},

            self() ! production_info,

            ogonek_mongo:planet_update_utilization(PlanetId, Updated),

            {noreply, State0};
        skipped ->
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_info({get_buildings, Silent}, State) ->
    case State#state.buildings of
        % buildings not fetched yet
        [] ->
            UserId = user_id(State),
            PlanetId = planet_id(State),

            Fetched = ogonek_mongo:buildings_of_planet(PlanetId),
            lager:debug("user ~s - fetched buildings of planet ~s: ~p",
                        [UserId, PlanetId, Fetched]),

            Capacity = ogonek_capacity:from_buildings(PlanetId, Fetched),
            State0 = State#state{buildings=Fetched, capacity=Capacity},

            json_to_sockets(ogonek_building, State0#state.buildings, State0, Silent),
            json_to_sockets(ogonek_capacity, Capacity, State0, Silent),

            {noreply, State0};
        % buildings already present
        _Otherwise ->
            json_to_sockets(ogonek_building, State#state.buildings, State, Silent),
            json_to_sockets(ogonek_capacity, State#state.capacity, State, Silent),
            {noreply, State}
    end;

handle_info({get_constructions, Silent}, State) ->
    case State#state.constructions of
        % constructions not fetched yet
        [] ->
            UserId = user_id(State),
            PlanetId = planet_id(State),
            Fetched = ogonek_mongo:constructions_of_planet(PlanetId),

            lager:debug("user ~s - fetched constructions of planet ~s: ~p",
                        [UserId, PlanetId, Fetched]),

            State0 = process_constructions(State, Fetched),

            trigger_construction_checks(State0#state.constructions),

            json_to_sockets(ogonek_construction, State0#state.constructions, State0, Silent),

            {noreply, State0};
        % constructions already present
        _Otherwise ->
            json_to_sockets(ogonek_construction, State#state.constructions, State, Silent),
            {noreply, State}
    end;

handle_info({get_weapon_orders, Silent}, State) ->
    case State#state.weapon_orders of
        % weapon orders not fetched yet
        [] ->
            UserId = user_id(State),
            PlanetId = planet_id(State),
            Fetched = ogonek_mongo:weapon_orders_of_planet(PlanetId),

            lager:debug("user ~s - fetched weapon orders of planet ~s: ~p",
                        [UserId, PlanetId, Fetched]),

            State0 = State#state{weapon_orders=Fetched},
            State1 = process_weapon_orders(State0, Fetched),

            trigger_weapon_order_checks(State1#state.weapon_orders),

            json_to_sockets(ogonek_weapon_order, State1#state.weapon_orders, State1, Silent),

            {noreply, State1};
        % weapon orders already present
        Orders ->
            json_to_sockets(ogonek_weapon_order, Orders, State, Silent),
            {noreply, State}
    end;

handle_info({get_ship_orders, Silent}, State) ->
    case State#state.ship_orders of
        % ship orders not fetched yet
        [] ->
            UserId = user_id(State),
            PlanetId = planet_id(State),
            Fetched = ogonek_mongo:ship_orders_of_planet(PlanetId),

            lager:debug("user ~s - fetched ship orders of planet ~s: ~p",
                        [UserId, PlanetId, Fetched]),

            State0 = State#state{ship_orders=Fetched},
            State1 = process_ship_orders(State0, Fetched),

            trigger_ship_order_checks(State1#state.ship_orders),

            json_to_sockets(ogonek_ship_order, State1#state.ship_orders, State1, Silent),

            {noreply, State1};
        % ship orders already present
        Orders ->
            json_to_sockets(ogonek_ship_order, Orders, State, Silent),
            {noreply, State}
    end;

handle_info(process_weapon_orders, State) ->
    UserId = user_id(State),
    Ws = State#state.weapon_orders,

    lager:debug("user ~s - processing weapon orders", [UserId]),

    State0 = process_weapon_orders(State, Ws),

    {noreply, State0};

handle_info(process_ship_orders, State) ->
    UserId = user_id(State),
    Ss = State#state.ship_orders,

    lager:debug("user ~s - processing ship orders", [UserId]),

    State0 = process_ship_orders(State, Ss),

    {noreply, State0};

handle_info(process_constructions, State) ->
    UserId = user_id(State),
    Cs = State#state.constructions,

    lager:debug("user ~s - processing constructions", [UserId]),

    State0 = process_constructions(State, Cs),

    {noreply, State0};

handle_info(Info, State) ->
    UserId = user_id(State),
    lager:warning("user ~s - unhandled message: ~p", [UserId, Info]),
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
terminate(_Reason, State) ->
    lager:debug("terminating planet ~s", [planet_id(State)]),
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

-spec json_to_sockets(Json :: term(), state()) -> ok.
json_to_sockets(Json, State) ->
    Session = State#state.session,
    gen_server:cast(Session, {json_to_sockets, Json}).


-spec json_to_sockets(Module :: atom(), Json :: term(), state()) -> ok.
json_to_sockets(Module, Json, State) ->
    Session = State#state.session,
    gen_server:cast(Session, {json_to_sockets, Module, Json}).


-spec json_to_sockets(Module :: atom(), Json :: term(), state(), Silent :: boolean()) -> ok.
json_to_sockets(_Module, _Json, _State, true) -> ok;
json_to_sockets(Module, Json, State, _) ->
    json_to_sockets(Module, Json, State).


-spec schedule_recalculate_resources() -> ok.
schedule_recalculate_resources() ->
    Interval = ?OGONEK_REFRESH_RESOURCE_INTERVAL_SECS * 1000,
    erlang:send_after(Interval, self(), schedule_calc_resources),
    ok.


-spec update_building([building()], building()) -> [building()].
update_building(Buildings, #building{type=Type}=Building) ->
    case ogonek_buildings:get_building(Buildings, Type) of
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


-spec trigger_construction_checks([construction()]) -> ok.
trigger_construction_checks(Constructions) ->
    lists:foreach(fun(C) ->
                          DueIn = ogonek_util:seconds_since(C#construction.finish),
                          trigger_construction_check(DueIn + 1)
                  end, Constructions).


-spec trigger_construction_check(Seconds :: integer()) -> ok.
trigger_construction_check(Seconds) ->
    erlang:send_after(Seconds * 1000, self(), process_constructions),
    ok.


-spec trigger_weapon_order_checks([weapon_order()]) -> ok.
trigger_weapon_order_checks(WOrders) ->
    lists:foreach(fun(WO) ->
                          DueIn = ogonek_util:seconds_since(WO#weapon_order.finish),
                          trigger_weapon_order_check(DueIn + 1)
                  end, WOrders).


-spec trigger_weapon_order_check(Seconds :: integer()) -> ok.
trigger_weapon_order_check(Seconds) ->
    erlang:send_after(Seconds * 1000, self(), process_weapon_orders),
    ok.


-spec trigger_ship_order_checks([ship_order()]) -> ok.
trigger_ship_order_checks(SOrders) ->
    lists:foreach(fun(SO) ->
                          DueIn = ogonek_util:seconds_since(SO#ship_order.finish),
                          trigger_ship_order_check(DueIn + 1)
                  end, SOrders).


-spec trigger_ship_order_check(Seconds :: integer()) -> ok.
trigger_ship_order_check(Seconds) ->
    erlang:send_after(Seconds * 1000, self(), process_ship_orders),
    ok.


-spec weapons_info(PlanetId ::  binary(), Weapons :: weapon_map()) -> [weapon()].
weapons_info(PlanetId, Weapons) ->
    lists:foldl(fun(#wdef{name=Name}, Ws) ->
                        case maps:get(Name, Weapons, undefined) of
                            undefined ->
                                [#weapon{type=Name, count=0, planet=PlanetId} | Ws];
                            Weapon ->
                                [Weapon | Ws]
                        end
                end, [], ogonek_weapons:definitions()).


-spec remove_weapon_order(state(), PlanetId :: binary(), OrderId :: binary()) -> state().
remove_weapon_order(State, _PlanetId, OrderId) ->
    WeaponOrders = State#state.weapon_orders,
    Orders = lists:filter(fun(#weapon_order{id=Id}) -> Id /= OrderId end, WeaponOrders),
    State#state{weapon_orders=Orders}.


-spec remove_ship_order(state(), PlanetId :: binary(), OrderId :: binary()) -> state().
remove_ship_order(State, _PlanetId, OrderId) ->
    ShipOrders = State#state.ship_orders,
    Orders = lists:filter(fun(#ship_order{id=Id}) -> Id /= OrderId end, ShipOrders),
    State#state{ship_orders=Orders}.


-spec ships_info(PlanetId ::  binary(), Ships :: ship_map()) -> [ship()].
ships_info(PlanetId, Ships) ->
    lists:foldl(fun(#sdef{name=Name}, Ss) ->
                        case maps:get(Name, Ships, undefined) of
                            undefined ->
                                [#ship{type=Name, count=0, planet=PlanetId} | Ss];
                            Ship ->
                                [Ship | Ss]
                        end
                end, [], ogonek_ships:definitions()).


-spec calc_resources(state()) -> state().
calc_resources(State) ->
    calc_resources(State, ogonek_util:now8601()).


-spec calc_resources(state(), timestamp()) -> state().
calc_resources(State, RelativeTo) ->
    Planet = State#state.planet,
    Buildings = State#state.buildings,

    case calculate_resources(State, Buildings, RelativeTo) of
        skipped ->
            State;
        Resources ->
            ogonek_mongo:planet_update_resources(Planet#planet.id, Resources),

            Planet0 = Planet#planet{resources=Resources},
            State#state{planet=Planet0}
    end.


-spec calculate_resources(state(), [building()], timestamp()) -> resources() | skipped.
calculate_resources(State, Buildings, RelativeTo) ->
    calculate_resources(State, Buildings, RelativeTo, false).


-spec calculate_resources(state(), [building()], timestamp(), boolean()) -> resources() | skipped.
calculate_resources(State, Buildings, RelativeTo, Force) ->
    UserId = user_id(State),
    Planet = State#state.planet,
    Resources = Planet#planet.resources,
    Capacity = State#state.capacity,

    SecondsSince = ogonek_util:seconds_since(Resources#resources.updated, RelativeTo),

    if SecondsSince >= 60 orelse Force == true ->
           Utilization = Planet#planet.utilization,
           Production = ogonek_production:of_planet(Planet, Buildings),
           lager:debug("user ~s - production: ~p", [UserId, Production]),

           SimulatedHours = ogonek_util:to_hours(SecondsSince, ?OGONEK_DEFAULT_ACCELERATION),

           Produced = ogonek_resources:with_factor(SimulatedHours, Production),
           lager:debug("user ~s - produced since ~s: ~p", [UserId, Resources#resources.updated, Produced]),

           Summed = ogonek_resources:sum(Resources, Produced),
           AfterConsumption = ogonek_buildings:calculate_building_consumption(Summed, Utilization, Buildings, SimulatedHours),

           lager:debug("user ~s - after consumption: ~p", [UserId, AfterConsumption]),

           Capped = ogonek_resources:with_capacity(AfterConsumption, Capacity),
           Capped#resources{updated=RelativeTo};
       true ->
           lager:debug("user ~s - skipping resources calculation [~p sec ago]",
                       [UserId, SecondsSince]),
           skipped
    end.


-spec update_power_workers(state()) -> state().
update_power_workers(State) ->
    Planet = State#state.planet,
    Buildings = State#state.buildings,
    Constructions = State#state.constructions,

    {Power, Workers} = ogonek_buildings:calculate_power_workers(Buildings, Constructions),

    Res = Planet#planet.resources,
    Res1 = Res#resources{power=Power, workers=Workers},
    Planet0 = Planet#planet{resources=Res1},
    State#state{planet=Planet0}.


-spec construction_possible(PlanetState :: state(), [research()], Costs :: bdef()) -> boolean().
construction_possible(PlanetState, Research, Costs) ->
    Planet = PlanetState#state.planet,
    Constructions = PlanetState#state.constructions,
    Buildings = PlanetState#state.buildings,
    Resources = Planet#planet.resources,

    NumConstructions = length(Constructions),
    MaxConcurrentConstructions = max_concurrent_constructions(Buildings),

    Requirements = Costs#bdef.requirements,

    % check if research and building requirements are met
    ogonek_research:has_requirements(Research, Requirements) andalso
    ogonek_buildings:has_requirements(Buildings, Requirements) andalso

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


-spec weapon_order_possible(state(), wdef()) -> boolean().
weapon_order_possible(State, WDef) ->
    Planet = State#state.planet,
    Buildings = State#state.buildings,
    Resources = Planet#planet.resources,

    Resources#resources.iron_ore >= WDef#wdef.iron_ore andalso
    Resources#resources.gold >= WDef#wdef.gold andalso
    Resources#resources.h2o >= WDef#wdef.h2o andalso
    Resources#resources.oil >= WDef#wdef.oil andalso
    Resources#resources.h2 >= WDef#wdef.h2 andalso
    Resources#resources.uranium >= WDef#wdef.uranium andalso
    Resources#resources.pvc >= WDef#wdef.pvc andalso
    Resources#resources.titan >= WDef#wdef.titan andalso
    Resources#resources.kyanite >= WDef#wdef.kyanite andalso

    has_weapon_manufacture(Buildings) == true.


-spec ship_order_possible(state(), sdef()) -> boolean().
ship_order_possible(State, SDef) ->
    Planet = State#state.planet,
    Buildings = State#state.buildings,
    Resources = Planet#planet.resources,

    Resources#resources.iron_ore >= SDef#sdef.iron_ore andalso
    Resources#resources.gold >= SDef#sdef.gold andalso
    Resources#resources.h2o >= SDef#sdef.h2o andalso
    Resources#resources.oil >= SDef#sdef.oil andalso
    Resources#resources.h2 >= SDef#sdef.h2 andalso
    Resources#resources.uranium >= SDef#sdef.uranium andalso
    Resources#resources.pvc >= SDef#sdef.pvc andalso
    Resources#resources.titan >= SDef#sdef.titan andalso
    Resources#resources.kyanite >= SDef#sdef.kyanite andalso

    has_space_shipyard(Buildings) == true.


-spec max_concurrent_constructions([building()]) -> integer().
max_concurrent_constructions(Buildings) ->
    CCLevel = construction_center_level(Buildings),
    CCLevel div 10 + 1.


-spec construction_center_level([building()]) -> integer().
construction_center_level(Buildings) ->
    ogonek_buildings:get_building_level(Buildings, construction_center).


-spec has_weapon_manufacture([building()]) -> boolean().
has_weapon_manufacture(Buildings) ->
    ogonek_buildings:get_building_level(Buildings, weapon_manufacture) > 0.


-spec has_space_shipyard([building()]) -> boolean().
has_space_shipyard(Buildings) ->
    ogonek_buildings:get_building_level(Buildings, space_shipyard) > 0.


-spec claim_resources(State :: state(), Costs :: bdef() | wdef() | sdef()) -> state().
claim_resources(State, Costs) ->
    Planet = State#state.planet,
    Resources = Planet#planet.resources,

    % claim_resources is called immediately on the creation of the construction
    % that's why we claim only the 'positive' costs at this point
    % see 'ogonek_resources:substract_costs'
    Res0 = ogonek_resources:substract_costs(Resources, Costs),

    Planet0 = Planet#planet{resources=Res0},
    State#state{planet=Planet0}.


-spec fetch_weapons(PlanetId :: binary()) -> weapon_map().
fetch_weapons(PlanetId) ->
    Weapons = ogonek_mongo:weapons_of_planet(PlanetId),

    lists:foldl(fun(#weapon{type=Type}=Weapon, Ws) ->
                        maps:put(Type, Weapon, Ws)
                end, maps:new(), Weapons).


-spec fetch_ships(PlanetId :: binary()) -> ship_map().
fetch_ships(PlanetId) ->
    Ships = ogonek_mongo:ships_of_planet(PlanetId),

    lists:foldl(fun(#ship{type=Type}=Ship, Ss) ->
                        maps:put(Type, Ship, Ss)
                end, maps:new(), Ships).


-spec process_constructions(state(), [construction()]) -> state().
process_constructions(State, []) -> State;
process_constructions(State, Constructions) ->
    {Finished, Running} = split_constructions(Constructions),

    PState0 = lists:foldl(fun process_construction/2, State, Finished),
    PState0#state{constructions=Running}.


-spec process_construction({construction(), timestamp()}, state()) -> state().
process_construction({Construction, UpTo}, State) ->
    Planet = State#state.planet,
    Buildings = State#state.buildings,
    PlanetId = Planet#planet.id,
    Type = Construction#construction.building,

    case ogonek_buildings:get_building(Buildings, Type) of
        {ok, B} ->
            CLevel = Construction#construction.level,
            BLevel = B#building.level,
            if BLevel + 1 == CLevel ->
                   Update = B#building{level=CLevel},

                   % trigger asynchronous db update
                   ogonek_mongo:building_finish(Update),

                   % in order to properly calculate multiple successive
                   % constructions we have to re-calculate resources
                   % in here already
                   Buildings0 = update_building(Buildings, Update),
                   Capacity = ogonek_capacity:from_buildings(PlanetId, Buildings0),

                   State0 = State#state{
                              buildings=Buildings0,
                              capacity=Capacity},

                   calc_resources(State0, UpTo);
               true ->
                   lager:warning("invalid construction building level: ~p",
                                 [Construction]),
                   State
            end;
        undefined ->
            lager:warning("construction finished for unknown building: ~p",
                          [Construction]),
            State
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


-spec process_weapon_orders(state(), [weapon_order()]) -> state().
process_weapon_orders(State, []) -> State;
process_weapon_orders(State, WOrders) ->
    Now = ogonek_util:now8601(),
    {Finished, Running} = lists:partition(fun(#weapon_order{finish=F}) ->
                                                  F =< Now
                                          end, WOrders),

    Ws = lists:foldl(fun(WOrder, Weapons) ->
                             lager:info("planet ~s - finishing weapon order ~p",
                                        [WOrder#weapon_order.planet, WOrder]),
                             finish_weapon_order(Weapons, WOrder)
                     end, State#state.weapons, Finished),

    State#state{weapon_orders=Running, weapons=Ws}.


-spec finish_weapon_order(Weapons :: #{atom() => weapon()}, weapon_order()) -> #{atom() => weapon()}.
finish_weapon_order(Weapons, WOrder) ->
    Planet = WOrder#weapon_order.planet,
    Weapon = WOrder#weapon_order.weapon,

    Updated =
    case maps:get(Weapon, Weapons, undefined) of
        undefined ->
            % new weapon
            #weapon{planet=Planet, type=Weapon, count=1};
        Existing ->
            % increment existing weapon's count
            Existing#weapon{count=Existing#weapon.count+1}
    end,

    ogonek_mongo:weapon_update(Updated, WOrder#weapon_order.id),

    maps:put(Weapon, Updated, Weapons).


-spec process_ship_orders(state(), [ship_order()]) -> state().
process_ship_orders(State, []) -> State;
process_ship_orders(State, SOrders) ->
    Now = ogonek_util:now8601(),
    {Finished, Running} = lists:partition(fun(#ship_order{finish=F}) ->
                                                  F =< Now
                                          end, SOrders),

    Ss = lists:foldl(fun(SOrder, Ships) ->
                             lager:info("planet ~s - finishing ship order ~p",
                                        [SOrder#ship_order.planet, SOrder]),
                             finish_ship_order(Ships, SOrder)
                     end, State#state.ships, Finished),

    State#state{ship_orders=Running, ships=Ss}.


-spec finish_ship_order(Ships :: #{atom() => ship()}, ship_order()) -> #{atom() => ship()}.
finish_ship_order(Ships, SOrder) ->
    Planet = SOrder#ship_order.planet,
    Ship = SOrder#ship_order.ship,

    Updated =
    case maps:get(Ship, Ships, undefined) of
        undefined ->
            % new ship
            #ship{planet=Planet, type=Ship, count=1};
        Existing ->
            % increment existing ship's count
            Existing#ship{count=Existing#ship.count+1}
    end,

    ogonek_mongo:ship_update(Updated, SOrder#ship_order.id),

    maps:put(Ship, Updated, Ships).


-spec planet_id(state()) -> binary().
planet_id(#state{planet=#planet{id=PlanetId}}) ->
    PlanetId.


-spec user_id(state()) -> binary().
user_id(#state{user=UserId}) ->
    UserId.


-spec resources(state()) -> resources().
resources(#state{planet=#planet{resources=Resources}}) ->
    Resources.


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
