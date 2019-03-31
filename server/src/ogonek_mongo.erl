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

-module(ogonek_mongo).

-include("include/ogonek.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Utils
-export([to_id/1,
         from_id/1]).

%% Session API
-export([new_session/2,
         refresh_session/1,
         get_session/1,
         add_user_to_session/2,
         remove_user_from_session/1]).

%% User API
-export([create_user_from_twitch/2,
         update_user/1,
         get_user/1,
         get_user/2]).

%% Buildings API
-export([building_finish/1,
         buildings_of_planet/1]).

%% Research API
-export([research_of_user/1,
         research_finish/1,
         research_create/1]).

%% Construction API
-export([construction_create/1,
         constructions_of_planet/1,
         construction_remove/3]).

%% Weapon Order API
-export([weapon_order_create/1,
         weapon_order_remove/1,
         weapon_orders_of_planet/1]).

%% Weapon API
-export([weapon_update/2,
         weapons_of_planet/1]).

%% Ships API
-export([ship_update/2,
         ships_of_planet/1]).

%% Ship Order API
-export([ship_order_create/1,
         ship_orders_of_planet/1,
         ship_order_remove/1]).

%% Planet API
-export([planet_exists/1,
         planet_exists/3,
         planet_create/1,
         planet_claim/2,
         planets_of_user/1,
         planet_update_utilization/2,
         planet_update_resources/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(MONGO_DB, <<"ogonek">>).

-record(state, {
          topology :: pid() | undefined
         }).

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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec new_session(RemoteIP :: binary(), Headers :: [kvalue()]) ->
    {ok, binary()} |
    {error, missing_id} |
    {error, missing_rev}.
new_session(RemoteIP, Headers) ->
    Info = get_info(),
    Now = ogonek_util:now8601(),
    Result = mongo_api:insert(Info, <<"session">>,
                              #{<<"headers">> => Headers,
                                <<"updated">> => Now,
                                <<"created">> => Now,
                                <<"ip">> => RemoteIP
                               }),
    case Result of
        {{true, _N}, #{<<"_id">> := Id}} -> {ok, objectid_to_binary(Id)};
        Otherwise ->
            lager:error("mongo - ~p", [Otherwise]),
            {error, missing_id}
    end.


-spec get_session(SessionId :: binary()) -> {ok, session()} | {error, not_found} | {error, invalid}.
get_session(SessionId) ->
    Info = get_info(),
    Result = mongo_api:find_one(Info, <<"session">>, id_query(SessionId), #{}),
    case Result of
        undefined ->
            {error, not_found};
        Session ->
            ogonek_session:from_doc(Session)
    end.


-spec refresh_session(SessionId :: binary()) -> ok.
refresh_session(SessionId) ->
    gen_server:cast(?MODULE, {refresh_session, SessionId}).


-spec remove_user_from_session(binary() | undefined) -> ok.
remove_user_from_session(undefined) -> ok;
remove_user_from_session(SessionId) ->
    gen_server:cast(?MODULE, {remove_user_from_session, SessionId}).


-spec add_user_to_session(UserId :: binary(), SessionId :: binary()) -> ok.
add_user_to_session(UserId, SessionId) ->
    gen_server:cast(?MODULE, {add_user_to_session, UserId, SessionId}).


-spec create_user_from_twitch(twitch_user(), Provider :: binary()) ->
    {ok, user()} |
    {error, invalid} |
    {error, missing_id} |
    {error, missing_rev}.
create_user_from_twitch(User, Provider) ->
    Info = get_info(),
    Map = #{<<"provider">> => Provider,
            <<"pid">> => User#twitch_user.id,
            <<"email">> => User#twitch_user.email,
            <<"name">> => User#twitch_user.display_name,
            <<"img">> => User#twitch_user.profile_image_url},

    % TODO: missing oauth information

    Result = mongo_api:insert(Info, <<"user">>, Map),
    case Result of
        {{true, _N}, Doc} ->
            ogonek_user:from_doc(Doc);
        Otherwise ->
            lager:error("mongo - create_user_from_twitch: ~p", [Otherwise]),
            {error, invalid}
    end.


-spec get_user(Id :: binary()) ->
    {ok, user()} |
    {error, invalid} |
    {error, not_found}.
get_user(UserId) ->
    Info = get_info(),
    Result = mongo_api:find_one(Info, <<"user">>, id_query(UserId), #{}),
    case Result of
        undefined ->
            {error, not_found};
        User ->
            ogonek_user:from_doc(User)
    end.


-spec get_user(Id :: binary(), Provider :: binary()) ->
    {ok, user()} |
    {error, invalid} |
    {error, multiple} |
    {error, not_found} |
    error.
get_user(Id, Provider) ->
    Info = get_info(),
    Query = #{<<"pid">> => Id, <<"provider">> => Provider},
    Result = mongo_api:find_one(Info, <<"user">>, Query, #{}),
    case Result of
        undefined ->
            {error, not_found};
        User ->
            ogonek_user:from_doc(User)
    end.


-spec update_user(user()) -> ok.
update_user(User) ->
    gen_server:cast(?MODULE, {update_user, User}).


-spec building_finish(building()) -> ok.
building_finish(Building) ->
    gen_server:cast(?MODULE, {building_finish, Building, self()}).


-spec buildings_of_planet(PlanetId :: binary()) -> [building()].
buildings_of_planet(PlanetId) ->
    Info = get_info(),
    Query = #{<<"planet">> => to_id(PlanetId)},
    find_all(Info, <<"building">>, Query, fun ogonek_building:from_doc/1).


-spec research_of_user(UserId :: binary()) -> [research()].
research_of_user(UserId) ->
    Info = get_info(),
    Query = #{<<"user">> => to_id(UserId)},
    find_all(Info, <<"research">>, Query, fun ogonek_research:from_doc/1).


-spec research_create(research()) -> ok.
research_create(Research) ->
    gen_server:cast(?MODULE, {research_create, Research, self()}).


-spec research_finish(research()) -> ok.
research_finish(Research) ->
    gen_server:cast(?MODULE, {research_finish, Research, self()}).


-spec construction_create(construction()) -> ok.
construction_create(Construction) ->
    gen_server:cast(?MODULE, {construction_create, Construction, self()}).


-spec construction_remove(PlanetId :: binary(), Building :: atom(), Level :: integer()) -> ok.
construction_remove(PlanetId, Building, Level) ->
    gen_server:cast(?MODULE, {construction_remove, PlanetId, Building, Level}).


-spec constructions_of_planet(binary()) -> [construction()].
constructions_of_planet(PlanetId) ->
    Info = get_info(),
    Query = #{<<"planet">> => to_id(PlanetId)},
    find_all(Info, <<"construction">>, Query, fun ogonek_construction:from_doc/1).


-spec weapon_order_create(weapon_order()) -> ok.
weapon_order_create(WOrder) ->
    gen_server:cast(?MODULE, {weapon_order_create, WOrder, self()}).


-spec weapon_orders_of_planet(PlanetId :: binary()) -> [weapon_order()].
weapon_orders_of_planet(PlanetId) ->
    Info = get_info(),
    Query = #{<<"planet">> => to_id(PlanetId)},
    find_all(Info, <<"weapon_order">>, Query, fun ogonek_weapon_order:from_doc/1).


-spec weapon_order_remove(OrderId :: binary()) -> ok.
weapon_order_remove(OrderId) ->
    gen_server:cast(?MODULE, {weapon_order_remove, OrderId}).


-spec weapon_update(weapon(), OrderId :: maybe_unset_id()) -> ok.
weapon_update(Weapon, OrderId) ->
    gen_server:cast(?MODULE, {weapon_update, Weapon, OrderId, self()}).


-spec weapons_of_planet(PlanetId :: binary()) -> [weapon()].
weapons_of_planet(PlanetId) ->
    Info = get_info(),
    Query = #{<<"planet">> => to_id(PlanetId)},
    find_all(Info, <<"weapon">>, Query, fun ogonek_weapon:from_doc/1).


-spec ship_update(ship(), OrderId :: maybe_unset_id()) -> ok.
ship_update(Ship, OrderId) ->
    gen_server:cast(?MODULE, {ship_update, Ship, OrderId, self()}).


-spec ships_of_planet(PlanetId :: binary()) -> [ship()].
ships_of_planet(PlanetId) ->
    Info = get_info(),
    Query = #{<<"planet">> => to_id(PlanetId)},
    find_all(Info, <<"ship">>, Query, fun ogonek_ship:from_doc/1).


-spec ship_order_create(ship_order()) -> ok.
ship_order_create(SOrder) ->
    gen_server:cast(?MODULE, {ship_order_create, SOrder, self()}).


-spec ship_order_remove(OrderId :: binary()) -> ok.
ship_order_remove(OrderId) ->
    gen_server:cast(?MODULE, {ship_order_remove, OrderId}).


-spec ship_orders_of_planet(PlanetId :: binary()) -> [ship_order()].
ship_orders_of_planet(PlanetId) ->
    Info = get_info(),
    Query = #{<<"planet">> => to_id(PlanetId)},
    find_all(Info, <<"ship_order">>, Query, fun ogonek_ship_order:from_doc/1).


-spec planet_exists(planet()) -> boolean().
planet_exists(#planet{position=Pos}) ->
    {X, Y, Z} = Pos,
    planet_exists(X, Y, Z).


-spec planet_exists(integer(), integer(), integer()) -> boolean().
planet_exists(X, Y, Z) ->
    Info = get_info(),
    Query = #{<<"pos">> => #{<<"x">> => X, <<"y">> => Y, <<"z">> => Z}},
    Result = mongo_api:find_one(Info, <<"planet">>, Query, #{<<"_id">> => 1}),
    case Result of
        undefined -> false;
        _Otherwise -> true
    end.


-spec planet_create(Planet :: planet()) -> ok.
planet_create(Planet) ->
    gen_server:cast(?MODULE, {planet_create, Planet}).


-spec planet_claim(Planet :: planet(), UserId :: binary()) -> ok.
planet_claim(Planet, UserId) ->
    Planet0 = Planet#planet{owner=UserId},
    gen_server:cast(?MODULE, {planet_claim, Planet0, self()}).


-spec planets_of_user(UserId :: binary()) -> [planet()].
planets_of_user(UserId) ->
    Info = get_info(),
    Query = #{<<"owner">> => to_id(UserId)},
    find_all(Info, <<"planet">>, Query, fun ogonek_planet:from_doc/1).


-spec planet_update_resources(PlanetId :: binary(), resources()) -> ok.
planet_update_resources(PlanetId, Resources) ->
    Res = Resources#resources{updated=ogonek_util:now8601()},
    gen_server:cast(?MODULE, {planet_update_resources, PlanetId, Res}).


-spec planet_update_utilization(PlanetId :: binary(), Utilization :: resources()) -> ok.
planet_update_utilization(PlanetId, Utilization) ->
    Util = Utilization#resources{updated=ogonek_util:now8601()},
    gen_server:cast(?MODULE, {planet_update_utilization, PlanetId, Util}).


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
init([]) ->
    gen_server:cast(self(), prepare),
    {ok, #state{}}.

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
handle_call(get_info, _From, #state{topology=Topology}=State) ->
    {reply, Topology, State};

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
    lager:info("mongodb - preparing connection"),

    Type = unknown,
    Hosts = ["localhost:27017"],
    Options = [],
    WorkerOptions = [{database, ?MONGO_DB}],
    {ok, Topology} = mongo_api:connect(Type, Hosts, Options, WorkerOptions),

    lager:info("mongodb - successfully connected to topology ~p", [Topology]),

    {noreply, State#state{topology=Topology}};

handle_cast({remove_user_from_session, SessionId}, #state{topology=T}=State) ->
    Now = ogonek_util:now8601(),
    Update = #{<<"$set">> => #{<<"updated">> => Now},
               <<"$unset">> => #{<<"user_id">> => 1}},
    mongo_api:update(T, <<"session">>, id_query(SessionId), Update, #{}),

    {noreply, State};

handle_cast({add_user_to_session, UserId, SessionId}, #state{topology=T}=State) ->
    Now = ogonek_util:now8601(),
    Update = #{<<"$set">> => #{<<"updated">> => Now, <<"user_id">> => to_id(UserId)}},
    mongo_api:update(T, <<"session">>, id_query(SessionId), Update, #{}),

    {noreply, State};

handle_cast({refresh_session, SessionId}, #state{topology=T}=State) ->
    Now = ogonek_util:now8601(),
    Update = #{<<"$set">> => #{<<"updated">> => Now}},
    mongo_api:update(T, <<"session">>, id_query(SessionId), Update, #{}),

    {noreply, State};

handle_cast({update_user, User}, #state{topology=T}=State) ->
    UserMap = ogonek_user:to_doc(User),
    Update = #{<<"$set">> => maps:remove(<<"_id">>, UserMap)},
    mongo_api:update(T, <<"user">>, id_query(User#user.id), Update, #{}),

    {noreply, State};

handle_cast({building_finish, #building{id=undefined}=B, Sender}, #state{topology=T}=State) ->
    Doc = ogonek_building:to_doc(B),
    case insert(T, <<"building">>, Doc, fun ogonek_building:from_doc/1) of
        {ok, Building} ->
            Sender ! {building_finish, Building};
        Otherwise ->
            lager:error("mongo - building_finish: ~p ~p", [B, Otherwise])
    end,
    {noreply, State};

handle_cast({building_finish, Building, Sender}, #state{topology=T}=State) ->
    Now = ogonek_util:now8601(),
    Update = #{<<"$set">> => #{<<"updated">> => Now, <<"level">> => Building#building.level}},
    case mongo_api:update(T, <<"building">>, id_query(Building#building.id), Update, #{}) of
        {true, #{<<"n">> := 1}} ->
            Sender ! {building_finish, Building};
        Otherwise ->
            lager:error("mongo - building_finish: ~p ~p", [Building, Otherwise])
    end,

    {noreply, State};

handle_cast({research_create, #research{id=undefined}=R, Sender}, #state{topology=T}=State) ->
    Doc = ogonek_research:to_doc(R),
    case insert(T, <<"research">>, Doc, fun ogonek_research:from_doc/1) of
        {ok, Research} ->
            Sender ! {research_create, Research};
        Otherwise ->
            lager:error("mongo - research_create: ~p ~p", [R, Otherwise])
    end,
    {noreply, State};

handle_cast({research_create, Research, Sender}, #state{topology=T}=State) ->
    Update = #{<<"$set">> =>
               #{<<"created">> => Research#research.created,
                 <<"finish">> => Research#research.finish,
                 <<"level">> => Research#research.level,
                 <<"progress">> => Research#research.progress
                }},
    case mongo_api:update(T, <<"research">>, id_query(Research#research.id), Update, #{}) of
        {true, #{<<"n">> := 1}} ->
            Sender ! {research_create, Research};
        Otherwise ->
            lager:error("mongo - research_create: ~p ~p", [Research, Otherwise])
    end,

    {noreply, State};

handle_cast({research_finish, Research, Sender}, #state{topology=T}=State) ->
    Update = #{<<"$set">> => #{<<"progress">> => false}},
    case mongo_api:update(T, <<"research">>, id_query(Research#research.id), Update, #{}) of
        {true, #{<<"n">> := 1}} ->
            Sender ! {research_finish, Research};
        Otherwise ->
            lager:error("mongo - research_finish: ~p ~p", [Research, Otherwise])
    end,

    {noreply, State};

handle_cast({construction_create, Construction, Sender}, #state{topology=T}=State) ->
    Doc = ogonek_construction:to_doc(Construction),
    case insert(T, <<"construction">>, Doc, fun ogonek_construction:from_doc/1) of
        {ok, Constr} ->
            Sender ! {construction_create, Constr};
        Otherwise ->
            lager:error("mongo - construction_create: ~p ~p", [Construction, Otherwise])
    end,
    {noreply, State};

handle_cast({construction_remove, PlanetId, Building, Level}, #state{topology=T}=State) ->
    Delete = #{<<"planet">> => to_id(PlanetId),
               <<"building">> => Building,
               <<"level">> => #{<<"$lte">> => Level}},
    mongo_api:delete(T, <<"construction">>, Delete),
    {noreply, State};

handle_cast({weapon_order_create, WOrder, Sender}, #state{topology=T}=State) ->
    Doc = ogonek_weapon_order:to_doc(WOrder),
    case insert(T, <<"weapon_order">>, Doc, fun ogonek_weapon_order:from_doc/1) of
        {ok, Order} ->
            Sender ! {weapon_order_create, Order};
        Otherwise ->
            lager:error("mongo - weapon_order_create: ~p ~p", [WOrder, Otherwise])
    end,
    {noreply, State};

handle_cast({weapon_order_remove, OrderId}, #state{topology=T}=State) ->
    mongo_api:delete(T, <<"weapon_order">>, id_query(OrderId)),
    {noreply, State};

handle_cast({weapon_update, #weapon{id=undefined}=W, OrderId, Sender}, #state{topology=T}=State) ->
    Doc = ogonek_weapon:to_doc(W),
    case insert(T, <<"weapon">>, Doc, fun ogonek_weapon:from_doc/1) of
        {ok, Weapon} ->
            Sender ! {weapon_update, Weapon, OrderId};
        Otherwise ->
            lager:error("mongo - weapon_update: ~p ~p", [W, Otherwise])
    end,
    {noreply, State};

handle_cast({weapon_update, Weapon, OrderId, Sender}, #state{topology=T}=State) ->
    Update = #{<<"$set">> => #{<<"count">> => Weapon#weapon.count}},
    case mongo_api:update(T, <<"weapon">>, id_query(Weapon#weapon.id), Update, #{}) of
        {true, #{<<"n">> := 1}} ->
            Sender ! {weapon_update, Weapon, OrderId};
        Otherwise ->
            lager:error("mongo - weapon_update: ~p ~p", [Weapon, Otherwise])
    end,

    {noreply, State};

handle_cast({ship_update, #ship{id=undefined}=S, OrderId, Sender}, #state{topology=T}=State) ->
    Doc = ogonek_ship:to_doc(S),
    case insert(T, <<"ship">>, Doc, fun ogonek_ship:from_doc/1) of
        {ok, Ship} ->
            Sender ! {ship_update, Ship, OrderId};
        Otherwise ->
            lager:error("mongo - ship_update: ~p ~p", [S, Otherwise])
    end,
    {noreply, State};

handle_cast({ship_update, Ship, OrderId, Sender}, #state{topology=T}=State) ->
    Update = #{<<"$set">> => #{<<"count">> => Ship#ship.count}},
    case mongo_api:update(T, <<"ship">>, id_query(Ship#ship.id), Update, #{}) of
        {true, #{<<"n">> := 1}} ->
            Sender ! {ship_update, Ship, OrderId};
        Otherwise ->
            lager:error("mongo - ship_update: ~p ~p", [Ship, Otherwise])
    end,

    {noreply, State};

handle_cast({ship_order_create, SOrder, Sender}, #state{topology=T}=State) ->
    Doc = ogonek_ship_order:to_doc(SOrder),
    case insert(T, <<"ship_order">>, Doc, fun ogonek_ship_order:from_doc/1) of
        {ok, Order} ->
            Sender ! {ship_order_create, Order};
        Otherwise ->
            lager:error("mongo - ship_order_create: ~p ~p", [SOrder, Otherwise])
    end,
    {noreply, State};

handle_cast({ship_order_remove, OrderId}, #state{topology=T}=State) ->
    mongo_api:delete(T, <<"ship_order">>, id_query(OrderId)),
    {noreply, State};

handle_cast({planet_create, Planet}, #state{topology=T}=State) ->
    Doc = ogonek_planet:to_doc(Planet),
    Result = mongo_api:insert(T, <<"planet">>, Doc),
    case Result of
        {{true, _N}, _Doc} ->
            ok;
        Otherwise ->
            lager:error("mongo - planet_create: ~p", [Otherwise])
    end,

    {noreply, State};

handle_cast({planet_claim, #planet{id=undefined}=P, Sender}, #state{topology=T}=State) ->
    Doc = ogonek_planet:to_doc(P),
    case insert(T, <<"planet">>, Doc, fun ogonek_planet:from_doc/1) of
        {ok, Planet} ->
            Sender ! {planet_claim, Planet};
        Otherwise ->
            lager:error("mongo - planet_claim: ~p ~p", [P, Otherwise])
    end,
    {noreply, State};

handle_cast({planet_claim, Planet, Sender}, #state{topology=T}=State) ->
    Update = #{<<"$set">> => #{<<"owner">> => to_id(Planet#planet.owner)}},
    case mongo_api:update(T, <<"planet">>, id_query(Planet#planet.id), Update, #{}) of
        {true, #{<<"n">> := 1}} ->
            Sender ! {planet_claim, Planet};
        Otherwise ->
            lager:error("mongo - planet_claim: ~p ~p", [Planet, Otherwise])
    end,

    {noreply, State};

handle_cast({planet_update_resources, PlanetId, Resources}, #state{topology=T}=State) ->
    Update = #{<<"$set">> => #{<<"resources">> => ogonek_resources:to_doc(Resources)}},
    case mongo_api:update(T, <<"planet">>, id_query(PlanetId), Update, #{}) of
        {true, #{<<"n">> := 1}} ->
            ok;
        Otherwise ->
            lager:error("mongo - planet_update_resources: ~p ~p ~p",
                        [PlanetId, Resources, Otherwise])
    end,

    {noreply, State};

handle_cast({planet_update_utilization, PlanetId, Util}, #state{topology=T}=State) ->
    Update = #{<<"$set">> => #{<<"utilization">> => ogonek_resources:to_doc(Util)}},
    case mongo_api:update(T, <<"planet">>, id_query(PlanetId), Update, #{}) of
        {true, #{<<"n">> := 1}} ->
            ok;
        Otherwise ->
            lager:error("mongo - planet_update_utilization: ~p ~p ~p",
                        [PlanetId, Util, Otherwise])
    end,

    {noreply, State};

handle_cast(Msg, State) ->
    lager:warning("mongodb - unhandled message: ~p", [Msg]),
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
handle_info(_Info, State) ->
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
terminate(_Reason, #state{topology=undefined}) ->
    ok;

terminate(_Reason, #state{topology=Topology}) ->
    mongo_api:disconnect(Topology),
    ok;

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

get_info() ->
    gen_server:call(?MODULE, get_info).


-spec insert(Topology :: pid(), Collection :: binary(), Doc :: map(), fun((map()) -> {ok, map()} | {error, invalid})) -> {ok, map()} | error.
insert(Topology, Collection, Doc, Convert) ->
    case mongo_api:insert(Topology, Collection, Doc) of
        {{true, _N}, Result} ->
            case Convert(Result) of
                {error, invalid} ->
                    lager:warning("mongodb - failed to convert '~s' document: ~p",
                                  [Collection, Result]),
                    error;
                Success -> Success
            end;
        _Otherwise ->
            error
    end.


-spec find_all(pid(), binary(), map(), fun((map()) -> {ok, term()} | {error, invalid})) -> [term()].
find_all(Topology, Collection, Query, FromDoc) ->
    Results = case mongo_api:find(Topology, Collection, Query, #{}) of
                  [] -> [];
                  {ok, Cursor} ->
                      case mc_cursor:rest(Cursor) of
                          error -> [];
                          Rss -> Rss
                      end
              end,

    lists:flatmap(fun(Doc) ->
                          case FromDoc(Doc) of
                              {ok, Obj} -> [Obj];
                              _Otherwise -> []
                          end
                  end, Results).


-spec objectid_to_binary(bson:objectid()) -> binary().
objectid_to_binary({Id}) -> objectid_to_binary(Id, []).

objectid_to_binary(<<>>, Result) ->
    list_to_binary(lists:reverse(Result));
objectid_to_binary(<<Hex:8, Bin/binary>>, Result) ->
    SL1 = erlang:integer_to_list(Hex, 16),
    SL2 = case erlang:length(SL1) of
        1 -> ["0"|SL1];
        _ -> SL1
    end,
objectid_to_binary(Bin, [SL2|Result]).


-spec binary_to_objectid(binary()) -> bson:objectid().
binary_to_objectid(BS) -> binary_to_objectid(BS, []).

binary_to_objectid(<<>>, Result) ->
    {list_to_binary(lists:reverse(Result))};
binary_to_objectid(<<BS:2/binary, Bin/binary>>, Result) ->
binary_to_objectid(Bin, [erlang:binary_to_integer(BS, 16)|Result]).


-spec from_id(undefined | binary() | bson:objectid()) -> undefined | binary().
from_id(undefined) -> undefined;
from_id(Bin) when is_binary(Bin) -> Bin;
from_id(ObjId) -> objectid_to_binary(ObjId).


-spec to_id(undefined | binary() | bson:objectid()) -> undefined | binary() | bson:objectid().
to_id(undefined) -> undefined;
to_id(Bin) when is_binary(Bin) ->
    % if the given binary is not convertible in to a proper
    % object-id we will return the unmodified binary instead
    case binary_to_objectid(Bin) of
        {<<_:96>>}=Oid -> Oid;
        _Otherwise -> Bin
    end;
to_id(ObjectId) ->
    ObjectId.


-spec id_query(bson:objectid()) -> map().
id_query(BinOrObjectId) ->
    #{<<"_id">> => to_id(BinOrObjectId)}.
