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
         calculate_power/1,
         calculate_workers/1,
         calculate_power_workers/1]).


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
