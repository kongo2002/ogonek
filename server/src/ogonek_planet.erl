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
            {<<"resources">>, ogonek_resources:empty()}
           ],

    case ogonek_util:keys(Keys, Planet) of
        [Id, Type, Size, [X, Y, Z], Idx, Owner, Res] ->
            case {parse_type(Type), ogonek_resources:from_json(Res)} of
                {error, _} -> {error, invalid};
                {_, {error, _}} -> {error, invalid};
                {Type0, {ok, Resources}} ->
                    % set resources' planet-id for consistency's sake
                    Res0 = Resources#resources{planet=Id},
                    {ok, #planet{id=Id,
                                 type=Type0,
                                 size=Size,
                                 position={X, Y, Z},
                                 index=Idx,
                                 owner=Owner,
                                 resources=Res0}}
            end;
        _Otherwise ->
            {error, invalid}
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
              {<<"resources">>, Res}
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
    production(Planet#planet.type, Planet#planet.size).


-spec production(planet_type(), integer()) -> resources().
production(Type, Size) ->
    Factor = size_factor(Size),
    [Iron, Gold, H2O, Oil, H2, Uranium, PVC, Kyanite] = base_production(Type),

    #resources{iron_ore=round(Iron * Factor),
               gold=round(Gold * Factor),
               h2o=round(H2O * Factor),
               oil=round(Oil * Factor),
               h2=round(H2 * Factor),
               uranium=round(Uranium * Factor),
               pvc=round(PVC * Factor),
               kyanite=round(Kyanite * Factor),
               % bogus values
               power=0,
               workers=0
              }.


-spec base_production(planet_type()) -> [integer()].
base_production(earth) ->
    % IRON ORE , GOLD , H2O , OIL , H2 , URANIUM , PVC , KYANITE
    [200       , 80   , 100 , 150 , 10 , 15      , 25  , 5];
base_production(water) ->
    % IRON ORE , GOLD , H2O , OIL , H2 , URANIUM , PVC , KYANITE
    [80        , 150  , 200 , 100 , 25 , 10      , 15  , 4];
base_production(fire) ->
    % IRON ORE , GOLD , H2O , OIL , H2 , URANIUM , PVC , KYANITE
    [150       , 100  , 80  , 200 , 10 , 30      , 15  , 3];
base_production(ice) ->
    % IRON ORE , GOLD , H2O , OIL , H2 , URANIUM , PVC , KYANITE
    [100       , 200  , 150 , 80  , 25 , 15      , 10  , 7].


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
