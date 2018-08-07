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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(planet_state, {
          planet :: planet(),
          buildings :: [building()]
         }).

-type planet_state() :: #planet_state{}.

-record(state, {
          id :: binary(),
          planets :: #{binary() => planet_state()}
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
start_link(UserId) ->
    gen_server:start_link(?MODULE, UserId, []).

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
init(UserId) ->
    lager:info("initializing user lifecycle of '~s' [~p]", [UserId, self()]),
    State = #state{id=UserId, planets=maps:new()},

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

    Planets = fetch_planets(State),
    PlanetMap = lists:foldl(fun(#planet{id=Id}=P, Ps) ->
                                    PState = #planet_state{planet=P, buildings=[]},
                                    maps:put(Id, PState, Ps)
                            end, maps:new(), Planets),

    lager:debug("user '~s' has ~p planets: ~p",
                [UserId, maps:size(PlanetMap), maps:keys(PlanetMap)]),

    {noreply, State#state{planets=PlanetMap}};

handle_cast({get_buildings, Planet, Sender}, State) ->
    case maps:get(Planet, State#state.planets, undefined) of
        % unknown, invalid or foreign planet
        undefined ->
            {noreply, State};
        % buildings not fetched yet
        #planet_state{buildings=[]}=PState ->
            Fetched = ogonek_db:buildings_of_planet(Planet),

            lager:debug("user ~s - fetched building of planet ~s: ~p",
                        [State#state.id, Planet, Fetched]),

            PState0 = PState#planet_state{buildings=Fetched},
            Planets0 = maps:put(Planet, PState0, State#state.planets),

            % TODO: send to 'Sender' or via session-manager?
            Sender ! {buildings, PState0#planet_state.buildings},

            {noreply, State#state{planets=Planets0}};
        % buildings already present
        PState ->
            % TODO: send to 'Sender' or via session-manager?
            Sender ! {buildings, PState#planet_state.buildings},
            {noreply, State}
    end;

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
    {noreply, State};

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
            lager:info("user '~s' has no planets yet - assigning a free one now", [UserId]),
            {ok, Planet} = ogonek_planet_manager:claim_free_planet(UserId),
            bootstrap_free_planet(Planet),
            [Planet];
        Ps -> Ps
    end.


-spec bootstrap_free_planet(planet()) -> ok.
bootstrap_free_planet(Planet) ->
    lager:info("user '~s' - bootstrapping initial infrastructure on planet ~s",
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
                            lager:error("user ~s: there is no building definition for ~p - skipping",
                                        [Planet#planet.owner, B]);
                        Def ->
                            finish_building(Def, PlanetId)
                    end
            end,
    lists:foreach(Build, InitialBuildings).


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
