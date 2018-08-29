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

-module(ogonek_planet_manager).

-include("ogonek.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([claim_free_planet/1,
         user_planets/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-type planet_map() :: #{binary() => planet()}.

-record(state, {
          planets :: planet_map(),
          user_planets :: #{binary() => planet_map()}
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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec claim_free_planet(binary()) -> ok.
claim_free_planet(UserId) ->
    gen_server:cast(?MODULE, {claim_free_planet, UserId}).


-spec user_planets(binary()) -> [planet()].
user_planets(UserId) ->
    gen_server:call(?MODULE, {user_planets, UserId}).

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
    lager:info("initializing planet manager at ~p", [self()]),

    gen_server:cast(self(), prepare),
    {ok, #state{planets=maps:new(), user_planets=maps:new()}}.

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
handle_call({user_planets, UserId}, _From, State) ->
    UserPlanets = ogonek_db:planets_of_user(UserId),
    State0 = lists:foldl(fun track_planet/2, State, UserPlanets),
    {reply, UserPlanets, State0};

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
handle_cast({claim_free_planet, UserId}, State) ->
    claim_random_planet(UserId),
    {noreply, State};

handle_cast(prepare, State) ->
    self() ! create_planet,

    {noreply, State};

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
handle_info({planet_claim, Planet}=Msg, State) ->
    State0 = track_planet(Planet, State),

    % forward to user lifecycle
    ogonek_session_manager:publish_to_user(Planet#planet.owner, Msg),

    {noreply, State0};

handle_info(create_planet, State) ->
    % create a random planet
    Planet = random_planet(false),

    case ogonek_db:planet_exists(Planet) of
        false -> ogonek_db:planet_create(Planet);
        true -> ok
    end,

    erlang:send_after(60 * 60 * 1000, self(), create_planet),

    {noreply, State};

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

-spec random_planet() -> planet().
random_planet() ->
    random_planet(true).


-spec random_planet(ForUser :: boolean()) -> planet().
random_planet(ForUser) ->
    [X, Y, Z] = [rand:uniform(100) || _ <- lists:seq(1, 3)],
    Size = case ForUser of
               true -> 3;
               _ -> rand:uniform(4)
           end,
    Index = rand:uniform(8),
    Type = ogonek_util:choose_random([earth, water, fire, ice]),
    #planet{type=Type,
            size=Size,
            position={X, Y, Z},
            index=Index,
            resources=ogonek_resources:empty()}.


-spec claim_random_planet(UserId :: binary()) -> ok.
claim_random_planet(UserId) ->
    Planet = random_planet(),
    case ogonek_db:planet_exists(Planet) of
        false ->
            % TODO: race between 'exists' and 'claim'
            ogonek_db:planet_claim(Planet, UserId),
            ok;
        true ->
            claim_random_planet(UserId)
    end.


-spec track_planet(planet(), state()) -> state().
track_planet(Planet, State) ->
    Planets = maps:put(Planet#planet.id, Planet, State#state.planets),
    track_user_planet(Planet, State#state{planets=Planets}).


-spec track_user_planet(planet(), state()) -> state().
track_user_planet(#planet{owner=undefined}, State) -> State;
track_user_planet(#planet{owner=UserId}=Planet, State) ->
    Id = Planet#planet.id,
    Planets = State#state.user_planets,
    Planets0 = case maps:get(UserId, Planets, undefined) of
                   undefined ->
                       maps:put(UserId, maps:from_list([{Id, Planet}]), Planets);
                   UPlanets ->
                       maps:put(UserId, maps:put(Id, Planet, UPlanets), Planets)
               end,
    State#state{user_planets=Planets0}.
