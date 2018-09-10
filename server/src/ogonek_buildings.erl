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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([definitions/0,
         definitions_map/0,
         get_definition/1,
         get_building/2,
         get_building_level/2,
         get_building_max_level/2,
         to_building_type/1,
         try_building_type/1,
         unlocked_buildings/2,
         calculate_power/1,
         calculate_workers/1,
         calculate_power_workers/2,
         apply_building_consumption/3,
         has_requirements/2,
         calculate_building_consumption/4,
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


-spec get_building([building()], Type :: atom()) -> {ok, building()} | undefined.
get_building(Buildings, Type) ->
    case lists:keyfind(Type, 4, Buildings) of
        false -> undefined;
        Building -> {ok, Building}
    end.


-spec get_buildings_of_type([building()], Type :: atom()) -> [building()].
get_buildings_of_type(Buildings, Type) ->
    lists:filter(fun(#building{type=T}) when T == Type -> true;
                    (_Otherwise) -> false
                 end, Buildings).


-spec get_building_level([building()], Type :: atom()) -> integer().
get_building_level(Buildings, Type) ->
    case get_building(Buildings, Type) of
        {ok, #building{level=Level}} -> Level;
        _Otherwise -> 0
    end.


-spec get_building_max_level([building()], Type :: atom()) -> integer().
get_building_max_level(Buildings, Type) ->
    case get_buildings_of_type(Buildings, Type) of
        [] -> 0;
        Bs ->
            Levels = lists:map(fun(#building{level=Lvl}) -> Lvl end, Bs),
            lists:max(Levels)
    end.


-spec has_requirement([building()], requirement()) -> boolean().
has_requirement(_Buildings, {research, _, _}) -> true;
has_requirement([], _Requirement) -> false;
has_requirement([#building{type=Type, level=Lvl} | Bs], {building, Name, MinLevel}=Req) ->
    if Type == Name andalso Lvl >= MinLevel ->
           true;
       true ->
           has_requirement(Bs, Req)
    end.


-spec has_requirements([building()], [requirement()]) -> boolean().
has_requirements(Buildings, Requirements) ->
    lists:all(fun(Req) -> has_requirement(Buildings, Req) end, Requirements).


-spec unlocked_buildings([building()], [research()]) -> [bdef()].
unlocked_buildings(Buildings, Research) ->
    StillLocked = lists:filter(fun(#bdef{name=Name}) ->
                                       not(lists:keymember(Name, 4, Buildings))
                               end, definitions()),

    lists:filter(fun(#bdef{requirements=Rs}) ->
                         % meet research requirements
                         ogonek_research:has_requirements(Research, Rs) andalso
                         % and meet building requirements as well
                         has_requirements(Buildings, Rs)
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


-spec calculate_power_workers([building()], [construction()]) -> {integer(), integer()}.
calculate_power_workers(Buildings, Constructions) ->
    Defs = definitions_map(),

    % at first we are going to calculate all power/worker consumption
    % based on finished buildings
    FromBuildings = lists:foldl(fun(#building{type=T, level=Lvl}, {Power, Workers}) ->
                                        Def = maps:get(T, Defs),
                                        Power0 = Power - Def#bdef.power * Lvl,
                                        Workers0 = Workers - Def#bdef.workers * Lvl,
                                        {Power0, Workers0}
                                end, {0, 0}, Buildings),

    % after that we will put those of ongoing constructions on top as well
    lists:foldl(fun(#construction{building=B}, {Power, Workers}) ->
                        Def = maps:get(B, Defs),
                        % we will take 'positive' costs into account only
                        Power0 = Power - max(Def#bdef.power, 0),
                        Workers0 = Workers - max(Def#bdef.workers, 0),
                        {Power0, Workers0}
                end, FromBuildings, Constructions).



-define(OGONEK_CHEMICAL_FACTORY_PROD, 10).
-define(OGONEK_SMELTING_PLANT_PROD, 13).
-define(OGONEK_PLASTIC_FACTORY_PROD, 15).
-define(OGONEK_CONSUMPTION_FACTOR, 2).


-spec apply_building_consumption(Resources :: resources(), Utilization :: resources(), [building()]) -> resources().
apply_building_consumption(Resources, Utilization, Buildings) ->
    lists:foldl(
      fun(#building{type=chemical_factory, level=L}, R) ->
              Prod = L * ?OGONEK_CHEMICAL_FACTORY_PROD * Utilization#resources.h2 div 100,
              R#resources{h2=R#resources.h2 + Prod,
                          h2o=R#resources.h2o - Prod * ?OGONEK_CONSUMPTION_FACTOR};
         (#building{type=plastic_factory, level=L}, R) ->
              Prod = L * ?OGONEK_PLASTIC_FACTORY_PROD * Utilization#resources.pvc div 100,
              R#resources{pvc=R#resources.pvc + Prod,
                          oil=R#resources.oil - Prod * ?OGONEK_CONSUMPTION_FACTOR};
         (#building{type=smelting_plant, level=L}, R) ->
              Prod = L * ?OGONEK_SMELTING_PLANT_PROD * Utilization#resources.titan div 100,
              R#resources{titan=R#resources.titan + Prod,
                          iron_ore=R#resources.iron_ore - Prod * ?OGONEK_CONSUMPTION_FACTOR};

         (_OtherBuilding, R) -> R
      end, Resources, Buildings).


-spec calculate_building_consumption(Resources :: resources(), Utilization :: resources(), [building()], TimeFactor :: float()) -> resources().
calculate_building_consumption(Resources, Utilization, Buildings, TimeFactor) ->
    % TODO: we need a proper distribution from level to production

    lists:foldl(
         % hydrogen
      fun(#building{type=chemical_factory, level=L}, R) ->
              Util = Utilization#resources.h2 / 100,
              Prod = round(L * ?OGONEK_CHEMICAL_FACTORY_PROD * TimeFactor * Util),
              ToConsume = Prod * ?OGONEK_CONSUMPTION_FACTOR,
              Available = R#resources.h2o,
              if ToConsume > Available ->
                     ToProd = Available div ?OGONEK_CONSUMPTION_FACTOR,
                     R#resources{h2=R#resources.h2 + ToProd,
                                 h2o=0};
                 true ->
                     R#resources{h2=R#resources.h2 + Prod,
                                 h2o=R#resources.h2o - ToConsume}
              end;
         % pvc
         (#building{type=plastic_factory, level=L}, R) ->
              Util = Utilization#resources.pvc / 100,
              Prod = round(L * ?OGONEK_PLASTIC_FACTORY_PROD * TimeFactor * Util),
              ToConsume = Prod * ?OGONEK_CONSUMPTION_FACTOR,
              Available = R#resources.oil,
              if ToConsume > Available ->
                     ToProd = Available div ?OGONEK_CONSUMPTION_FACTOR,
                     R#resources{pvc=R#resources.pvc + ToProd,
                                 oil=0};
                 true ->
                     R#resources{pvc=R#resources.pvc + Prod,
                                 oil=R#resources.oil - ToConsume}
              end;
         % titan
         (#building{type=smelting_plant, level=L}, R) ->
              Util = Utilization#resources.titan / 100,
              Prod = round(L * ?OGONEK_SMELTING_PLANT_PROD * TimeFactor * Util),
              ToConsume = Prod * ?OGONEK_CONSUMPTION_FACTOR,
              Available = R#resources.iron_ore,
              if ToConsume > Available ->
                     ToProd = Available div ?OGONEK_CONSUMPTION_FACTOR,
                     R#resources{titan=R#resources.titan + ToProd,
                                 iron_ore=0};
                 true ->
                     R#resources{titan=R#resources.titan + Prod,
                                 iron_ore=R#resources.iron_ore - ToConsume}
              end;

         (_OtherBuilding, R) -> R
      end, Resources, Buildings).

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
         % kyanite
         (#building{type=kyanite_mine, level=L}, R) ->
              R#resources{kyanite=R#resources.kyanite + L};

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


% TODO: rather move into buildings configuration
% so we can't forget this for some building
-spec base_construction_duration(atom()) -> integer().
base_construction_duration(construction_center) -> 15000;
base_construction_duration(research_lab) -> 8000;
base_construction_duration(oil_rig) -> 1000;
base_construction_duration(oil_tank) -> 800;
base_construction_duration(water_rig) -> 1000;
base_construction_duration(water_tank) -> 800;
base_construction_duration(ore_mine) -> 1000;
base_construction_duration(ore_depot) -> 800;
base_construction_duration(gold_mine) -> 1000;
base_construction_duration(gold_depot) -> 800;
base_construction_duration(uranium_mine) -> 1500;
base_construction_duration(uranium_depot) -> 1000;
base_construction_duration(kyanite_mine) -> 1750;
base_construction_duration(kyanite_depot) -> 1100;
base_construction_duration(plastic_factory) -> 1450;
base_construction_duration(pvc_depot) -> 1000;
base_construction_duration(smelting_plant) -> 1450;
base_construction_duration(titan_depot) -> 1000;
base_construction_duration(chemical_factory) -> 1450;
base_construction_duration(h2_depot) -> 1000;
base_construction_duration(power_plant) -> 1000;
base_construction_duration(wind_turbine) -> 1200;
base_construction_duration(hydro_plant) -> 3500;
base_construction_duration(apartment) -> 600;
base_construction_duration(apartment_block) -> 1100;
base_construction_duration(apartment_complex) -> 4000;
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

%%
%% TESTS
%%

-ifdef(TEST).

calculate_building_consumption_test_() ->
    PId = <<"planet">>,
    Now = ogonek_util:now8601(),
    Empty = ogonek_resources:empty(),
    Hour = 1.0,
    ThreeHours = 3.0,
    Utilization = Empty#resources{titan=100, pvc=100, h2=100},
    HalfUtil = Empty#resources{titan=50, pvc=50, h2=50},
    Smelting1 = #building{planet=PId, type=smelting_plant, level=1, created=Now},
    Plastic1 = #building{planet=PId, type=plastic_factory, level=1, created=Now},
    Chemic1 = #building{planet=PId, type=chemical_factory, level=1, created=Now},

    [% no consumption/production whatsoever
     ?_assertEqual(Empty, calculate_building_consumption(Empty, Utilization, [Smelting1], Hour)),
     ?_assertEqual(Empty, calculate_building_consumption(Empty, Utilization, [Plastic1], Hour)),
     ?_assertEqual(Empty, calculate_building_consumption(Empty, Utilization, [Chemic1], Hour)),
     % enough base resources
     ?_assertEqual(Empty#resources{iron_ore=974, titan=13},
                   calculate_building_consumption(Empty#resources{iron_ore=1000}, Utilization, [Smelting1], Hour)),
     ?_assertEqual(Empty#resources{oil=970, pvc=15},
                   calculate_building_consumption(Empty#resources{oil=1000}, Utilization, [Plastic1], Hour)),
     ?_assertEqual(Empty#resources{h2o=980, h2=10},
                   calculate_building_consumption(Empty#resources{h2o=1000}, Utilization, [Chemic1], Hour)),
     % *not* enough base resources
     ?_assertEqual(Empty#resources{iron_ore=0, titan=13},
                   calculate_building_consumption(Empty#resources{iron_ore=26}, Utilization, [Smelting1], ThreeHours)),
     ?_assertEqual(Empty#resources{oil=0, pvc=15},
                   calculate_building_consumption(Empty#resources{oil=30}, Utilization, [Plastic1], ThreeHours)),
     ?_assertEqual(Empty#resources{h2o=0, h2=10},
                   calculate_building_consumption(Empty#resources{h2o=20}, Utilization, [Chemic1], ThreeHours)),
     % no consumption at all
     ?_assertEqual(Empty#resources{h2o=1000},
                   calculate_building_consumption(Empty#resources{h2o=1000}, Utilization, [], Hour)),
     % half utilization
     ?_assertEqual(Empty#resources{h2o=990, h2=5},
                   calculate_building_consumption(Empty#resources{h2o=1000}, HalfUtil, [Chemic1], Hour)),
     ?_assertEqual(Empty#resources{iron_ore=986, titan=7},
                   calculate_building_consumption(Empty#resources{iron_ore=1000}, HalfUtil, [Smelting1], Hour)),
     ?_assertEqual(Empty#resources{oil=984, pvc=8},
                   calculate_building_consumption(Empty#resources{oil=1000}, HalfUtil, [Plastic1], Hour))
    ].

has_requirements_test_() ->
    PId = <<"planet">>,
    Now = ogonek_util:now8601(),
    Smelting1 = #building{planet=PId, type=smelting_plant, level=1, created=Now},
    Plastic1 = #building{planet=PId, type=plastic_factory, level=1, created=Now},
    Chemic1 = #building{planet=PId, type=chemical_factory, level=1, created=Now},

    [?_assertEqual(true, has_requirements([Smelting1], [])),
     ?_assertEqual(false, has_requirements([Smelting1, Plastic1, Chemic1], [{building, oil_rig, 1}])),
     ?_assertEqual(true, has_requirements([Smelting1, Plastic1, Chemic1], [{building, smelting_plant, 1}]))
    ].

-endif.
