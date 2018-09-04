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

-module(ogonek_planet).

-include("ogonek.hrl").

-export([from_json/1,
         to_json/1,
         to_json/2,
         exists/1,
         exists/3,
         production/1]).


-spec from_json(json_doc()) -> {ok, planet()} | {error, invalid}.
from_json(Planet) ->
    Keys = [<<"_id">>, <<"type">>, <<"size">>, <<"pos">>, <<"idx">>,
            {<<"owner">>, undefined},
            {<<"resources">>, undefined},
            {<<"utilization">>, undefined}
           ],

    case ogonek_util:keys(Keys, Planet) of
        [Id, Type, Size, [X, Y, Z], Idx, Owner, Res, Util] when is_binary(Id) ->
            case {parse_type(Type), resources_or_empty(Id, Res), resources_or_empty(Id, Util)} of
                {error, _, _} -> {error, invalid};
                {_, {error, _}, _} -> {error, invalid};
                {_, _, {error, _}} -> {error, invalid};
                {Type0, {ok, Resources}, {ok, Utilization}} ->
                    {ok, #planet{id=Id,
                                 type=Type0,
                                 size=Size,
                                 position={X, Y, Z},
                                 index=Idx,
                                 owner=Owner,
                                 resources=Resources,
                                 utilization=Utilization}}
            end;
        _Otherwise ->
            {error, invalid}
    end.


-spec resources_or_empty(json_doc() | undefined) -> {ok, resources()} | {error, invalid}.
resources_or_empty(undefined) ->
    {ok, ogonek_resources:empty()};
resources_or_empty(ResJson) ->
    ogonek_resources:from_json(ResJson).


-spec resources_or_empty(PlanetId :: binary(), json_doc() | undefined) -> {ok, resources()} | {error, invalid}.
resources_or_empty(PlanetId, ResJson) ->
    case resources_or_empty(ResJson) of
        {ok, Res} -> {ok, Res#resources{planet=PlanetId}};
        Error -> Error
    end.


-spec to_json(planet()) -> tuple().
to_json(Planet) ->
    to_json(Planet, true).


-spec to_json(planet(), boolean()) -> tuple().
to_json(Planet, Db) ->
    {X, Y, Z} = Planet#planet.position,
    Res = ogonek_resources:to_json(Planet#planet.resources, Db),
    Values = [{<<"type">>, Planet#planet.type},
              {<<"size">>, Planet#planet.size},
              {<<"pos">>, [X, Y, Z]},
              {<<"idx">>, Planet#planet.index},
              {<<"resources">>, Res},
              {<<"utilization">>, ogonek_resources:to_json(Planet#planet.utilization, Db)}
             ]
    ++ ogonek_util:if_defined(<<"_id">>, Planet#planet.id)
    ++ ogonek_util:if_defined(<<"owner">>, Planet#planet.owner),
    ogonek_util:doc(<<"planet">>, Values).


-spec exists(coordinate()) -> boolean().
exists({X, Y, Z}) ->
    exists(X, Y, Z).


-spec exists(integer(), integer(), integer()) -> boolean().
exists(X, Y, Z) ->
    ogonek_db:planet_exists(X, Y, Z).


% we 'recycle' the resources record to represent the
% production rate per hour of all resources
-spec production(planet()) -> resources().
production(Planet) ->
    Prod = production(Planet#planet.type, Planet#planet.size),
    Prod#resources{planet=Planet#planet.id}.


-spec production(planet_type(), integer()) -> resources().
production(Type, Size) ->
    Factor = size_factor(Size),
    [Iron, Gold, H2O, Oil, Uranium, Kyanite] = base_production(Type),

    #resources{iron_ore=round(Iron * Factor),
               gold=round(Gold * Factor),
               h2o=round(H2O * Factor),
               oil=round(Oil * Factor),
               uranium=round(Uranium * Factor),
               kyanite=round(Kyanite * Factor),
               % pvc, titan and h2 is not depending on
               % planet's size or type
               pvc=1,
               titan=1,
               h2=1,
               % bogus values
               power=0,
               workers=0
              }.


-spec base_production(planet_type()) -> [integer()].
base_production(earth) ->
    % IRON ORE , GOLD , H2O , OIL , URANIUM , KYANITE
    [200       , 80   , 100 , 150 , 15      , 5];
base_production(water) ->
    % IRON ORE , GOLD , H2O , OIL , URANIUM , KYANITE
    [80        , 150  , 200 , 100 , 10      , 4];
base_production(fire) ->
    % IRON ORE , GOLD , H2O , OIL , URANIUM , KYANITE
    [150       , 100  , 80  , 200 , 30      , 3];
base_production(ice) ->
    % IRON ORE , GOLD , H2O , OIL , URANIUM , KYANITE
    [100       , 200  , 150 , 80  , 15      , 7].


-spec size_factor(integer()) -> float().
size_factor(1) -> 0.5;
size_factor(2) -> 0.7;
size_factor(3) -> 1.0;
size_factor(4) -> 1.3;
size_factor(_) -> 0.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_type(binary()) -> planet_type() | error.
parse_type(<<"earth">>) -> earth;
parse_type(<<"water">>) -> water;
parse_type(<<"fire">>) -> fire;
parse_type(<<"ice">>) -> ice;
parse_type(_Invalid) -> error.
