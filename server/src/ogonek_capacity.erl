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

-module(ogonek_capacity).

-include("ogonek.hrl").

-export([empty/1,
         from_buildings/2,
         to_json/1,
         to_json/2]).


-spec empty(PlanetId :: maybe_unset_id()) -> capacity().
empty(PlanetId) ->
    #capacity{
       planet=PlanetId,
       iron_ore=0,
       gold=0,
       h2o=0,
       oil=0,
       h2=0,
       uranium=0,
       pvc=0,
       titan=0,
       kyanite=0
      }.


-spec to_json(Capacity :: capacity()) -> tuple().
to_json(Capacity) ->
    to_json(Capacity, true).


-spec to_json(Capacity :: capacity(), Db :: boolean()) -> tuple().
to_json(Capacity, _Db) ->
    Values = [{<<"iron_ore">>, Capacity#capacity.iron_ore},
              {<<"gold">>, Capacity#capacity.gold},
              {<<"h2o">>, Capacity#capacity.h2o},
              {<<"oil">>, Capacity#capacity.oil},
              {<<"h2">>, Capacity#capacity.h2},
              {<<"uranium">>, Capacity#capacity.uranium},
              {<<"pvc">>, Capacity#capacity.pvc},
              {<<"titan">>, Capacity#capacity.titan},
              {<<"kyanite">>, Capacity#capacity.kyanite}
             ] ++ ogonek_util:if_defined(<<"planet">>, Capacity#capacity.planet),

    ogonek_util:doc(<<"capacity">>, Values).


-spec from_buildings(PlanetId :: binary(), Buildings :: [building()]) -> capacity().
from_buildings(PlanetId, Buildings) ->
    lists:foldl(fun(#building{type=ore_depot, level=Level}, Cap) ->
                        Cap#capacity{iron_ore=25000 * Level};
                   (#building{type=gold_depot, level=Level}, Cap) ->
                        Cap#capacity{gold=25000 * Level};
                   (#building{type=water_tank, level=Level}, Cap) ->
                        Cap#capacity{h2o=25000 * Level};
                   (#building{type=oil_tank, level=Level}, Cap) ->
                        Cap#capacity{oil=25000 * Level};
                   (_Building, Cap) ->
                        Cap
                end, empty(PlanetId), Buildings).
