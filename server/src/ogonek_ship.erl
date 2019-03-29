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

-module(ogonek_ship).

-include("include/ogonek.hrl").

-export([to_json/1,
         to_json/2,
         from_doc/1]).


-spec to_json(ship()) -> tuple().
to_json(Ship) ->
    to_json(Ship, true).


-spec to_json(ship(), boolean()) -> tuple().
to_json(Ship, Db) ->
    Values = [{<<"count">>, Ship#ship.count},
              {<<"planet">>, Ship#ship.planet}
             ],

    Values0 =
    case Db of
        true ->
            [{<<"type">>, Ship#ship.type} | Values]
            ++ ogonek_util:if_defined(<<"_id">>, Ship#ship.id);
        false ->
            SDef = ogonek_ships:get_definition(Ship#ship.type),

            % we recycle the 'db-mode' of ogonek_weapons:to_json in here
            % to omit the 't' tag in the generated json properties
            {Defs} = ogonek_ships:to_json(SDef, true),

            Values ++ Defs
    end,

    ogonek_util:doc(<<"ship">>, Values0).


-spec from_doc(Doc :: map()) -> {ok, ship()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"planet">> := PlanetId,
          <<"type">> := Type,
          <<"count">> := Count} ->
            {ok, #ship{id=maps:get(<<"_id">>, Doc, undefined),
                       planet=PlanetId,
                       type=ogonek_ships:to_ship_type(Type),
                       count=Count}};
        _Otherwise ->
            {error, invalid}
    end.
