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

-module(ogonek_production).

-include("include/ogonek.hrl").

-export([to_json/1,
         to_json/2,
         of_planet/2]).


-spec to_json(Production :: resources()) -> tuple().
to_json(Production) ->
    to_json(Production, true).


-spec to_json(Production :: resources(), Db :: boolean()) -> tuple().
to_json(Production, _Db) ->
    % we are going to reuse the resource's json transformation
    % in database-mode in order to omit the type field
    {Res} = ogonek_resources:to_json(Production, true),
    WithId = Res ++ ogonek_util:if_defined(<<"planet">>, Production#resources.planet),

    ogonek_util:doc(<<"production">>, WithId).


-spec of_planet(Planet :: planet(), [building()]) -> resources().
of_planet(Planet, Buildings) ->
    % the production capabilities of the planet's buildings are the
    % base of the overall resource production
    % that productivity is calculated in relation to the planet's base resources
    Production = ogonek_buildings:calculate_building_production(Buildings),
    PlanetResources = ogonek_planet:production(Planet),
    ogonek_resources:multiply(PlanetResources, Production).
