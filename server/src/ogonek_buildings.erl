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

-module(ogonek_buildings).

-include("ogonek.hrl").

-export([definitions/0,
         definitions_map/0,
         get_definition/1,
         to_building_type/1,
         try_building_type/1,
         unlocked_buildings/2,
         calculate_power/1,
         calculate_workers/1,
         calculate_power_workers/1,
         calculate_building_production/1,
         calculate_construction_duration/1,
         calculate_construction_duration/2,
         calculate_building_costs/1,
         calculate_building_costs/2]).


-spec definitions() -> [bdef()].
definitions() ->
    case application:get_env(buildings) of
        undefined -> [];
        {ok, Buildings} -> Buildings
    end.


-spec definitions_map() -> #{atom() => bdef()}.
definitions_map() ->
    lists:foldl(fun(#bdef{name=Name}=BDef, Bs) ->
                        maps:put(Name, BDef, Bs)
                end, maps:new(), definitions()).


-spec get_definition(atom()) -> bdef() | error.
get_definition(Name) ->
    get_definition(Name, definitions()).


-spec get_definition(atom(), [bdef()]) -> bdef() | error.
get_definition(_Name, []) -> error;
get_definition(Name, [#bdef{name=Name}=Def | _Ds]) -> Def;
get_definition(Name, [_ | Ds]) ->
    get_definition(Name, Ds).


-spec unlocked_buildings([building()], [research()]) -> [bdef()].
unlocked_buildings(Buildings, Research) ->
    StillLocked = lists:filter(fun(#bdef{name=Name}) ->
                                       not(lists:keymember(Name, 4, Buildings))
                               end, definitions()),

    lists:filter(fun(#bdef{requirements=Rs}) ->
                         ogonek_research:has_requirements(Research, Rs)
                 end, StillLocked).


-spec to_building_type(binary()) -> atom().
to_building_type(TypeName) when is_binary(TypeName) ->
    % this looks scary but the valid list of building types
    % should be already existing via configuration initialization
    erlang:binary_to_existing_atom(TypeName, utf8).


-spec try_building_type(binary()) -> atom() | {error, invalid}.
try_building_type(Type) when is_binary(Type) ->
    try
        to_building_type(Type)
    catch
        _Error -> {error, invalid}
    end;

try_building_type(_Type) ->
    {error, invalid}.


-spec calculate_power([building()]) -> integer().
calculate_power(Buildings) ->
    Defs = definitions_map(),
    lists:foldl(fun(#building{type=T, level=Lvl}, Power) ->
                        Def = maps:get(T, Defs),
                        Power - Def#bdef.power * Lvl
                end, 0, Buildings).


-spec calculate_workers([building()]) -> integer().
calculate_workers(Buildings) ->
    Defs = definitions_map(),
    lists:foldl(fun(#building{type=T, level=Lvl}, Workers) ->
                        Def = maps:get(T, Defs),
                        Workers - Def#bdef.workers * Lvl
                end, 0, Buildings).


-spec calculate_power_workers([building()]) -> {integer(), integer()}.
calculate_power_workers(Buildings) ->
    Defs = definitions_map(),
    lists:foldl(fun(#building{type=T, level=Lvl}, {Power, Workers}) ->
                        Def = maps:get(T, Defs),
                        Power0 = Power - Def#bdef.power * Lvl,
                        Workers0 = Workers - Def#bdef.workers * Lvl,
                        {Power0, Workers0}
                end, {0, 0}, Buildings).


-spec calculate_building_production([building()]) -> resources().
calculate_building_production(Buildings) ->
    % TODO: we need a proper distribution from level to production

    lists:foldl(
         % iron ore
      fun(#building{type=ore_mine, level=L}, R) ->
              R#resources{iron_ore=R#resources.iron_ore + L};
         (#building{type=ext_ore_mine, level=L}, R) ->
              R#resources{iron_ore=R#resources.iron_ore + L * 3};
         % gold
         (#building{type=gold_mine, level=L}, R) ->
              R#resources{gold=R#resources.gold + L};
         (#building{type=ext_gold_mine, level=L}, R) ->
              R#resources{gold=R#resources.gold + L * 3};
         % h2o
         (#building{type=water_rig, level=L}, R) ->
              R#resources{h2o=R#resources.h2o + L};
         (#building{type=ext_water_rig, level=L}, R) ->
              R#resources{h2o=R#resources.h2o + L * 3};
         % oil
         (#building{type=oil_rig, level=L}, R) ->
              R#resources{oil=R#resources.oil + L};
         (#building{type=ext_oil_rig, level=L}, R) ->
              R#resources{oil=R#resources.oil + L * 3};
         % uranium
         (#building{type=uranium_mine, level=L}, R) ->
              R#resources{uranium=R#resources.uranium + L};

         (_OtherBuilding, R) -> R
      end, ogonek_resources:empty(), Buildings).


-spec calculate_construction_duration(building()) -> integer().
calculate_construction_duration(#building{type=Type, level=Level}) ->
    calculate_construction_duration(Type, Level).


-spec calculate_construction_duration(atom(), integer()) -> integer().
calculate_construction_duration(Type, Level) ->
    % TODO: we need a proper distribution from level to duration
    BaseDuration = base_construction_duration(Type),
    LevelDuration = 50 * math:pow(Level, 1.5),
    round(BaseDuration + LevelDuration).


-spec base_construction_duration(atom()) -> integer().
base_construction_duration(construction_center) -> 15000;
base_construction_duration(research_lab) -> 8000;
base_construction_duration(oil_rig) -> 1000;
base_construction_duration(water_rig) -> 1000;
base_construction_duration(ore_mine) -> 1000;
base_construction_duration(gold_mine) -> 1000;
base_construction_duration(oil_tank) -> 800;
base_construction_duration(water_tank) -> 800;
base_construction_duration(uranium_mine) -> 1500;
base_construction_duration(uranium_depot) -> 1000;
base_construction_duration(ore_depot) -> 800;
base_construction_duration(gold_depot) -> 800;
base_construction_duration(power_plant) -> 1000;
base_construction_duration(wind_turbine) -> 1200;
base_construction_duration(hydro_plant) -> 3500;
base_construction_duration(apartment) -> 600;
base_construction_duration(apartment_block) -> 1100;
base_construction_duration(ext_oil_rig) -> 3000;
base_construction_duration(ext_water_rig) -> 3000;
base_construction_duration(ext_ore_mine) -> 3000;
base_construction_duration(ext_gold_mine) -> 3000.


-spec calculate_building_costs(Building :: building()) -> bdef() | error.
calculate_building_costs(#building{type=Type, level=Level}) ->
    case ogonek_buildings:get_definition(Type) of
        error -> error;
        Definition -> calculate_building_costs(Definition, Level)
    end.


-spec calculate_building_costs(Definition :: bdef(), Level :: integer()) -> bdef().
calculate_building_costs(Definition, Level) ->
    % TODO: we need a proper distribution from level to costs
    Factor = max(math:pow(Level, 1.2) * 0.5, 1.0),
    Definition#bdef{
      iron_ore=round(Definition#bdef.iron_ore * Factor),
      gold=round(Definition#bdef.gold * Factor),
      h2o=round(Definition#bdef.h2o * Factor),
      oil=round(Definition#bdef.oil * Factor),
      h2=round(Definition#bdef.h2 * Factor),
      uranium=round(Definition#bdef.uranium * Factor),
      pvc=round(Definition#bdef.pvc * Factor),
      titan=round(Definition#bdef.titan * Factor),
      kyanite=round(Definition#bdef.kyanite * Factor)
     }.
