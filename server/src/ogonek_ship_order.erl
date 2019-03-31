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

-module(ogonek_ship_order).

-include("include/ogonek.hrl").

-export([from_json/1,
         from_doc/1,
         to_doc/1,
         to_json/1,
         to_json/2]).


-spec from_json(json_doc()) -> {ok, ship_order()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"_id">>, <<"ship">>, <<"planet">>, <<"created">>, <<"finish">>],

    case ogonek_util:keys(Keys, Json) of
        [Id, Ship, Planet, Created, Finish] ->
            {ok, #ship_order{id=Id,
                             ship=ogonek_ships:to_ship_type(Ship),
                             planet=Planet,
                             created=Created,
                             finish=Finish}};
        _Otherwise ->
            {error, invalid}
    end.


-spec from_doc(Doc :: map()) -> {ok, ship_order()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"ship">> := Ship,
          <<"planet">> := PlanetId,
          <<"created">> := Created,
          <<"finish">> := Finish} ->
            {ok, #ship_order{id=maps:get(<<"_id">>, Doc, undefined),
                             ship=ogonek_ships:to_ship_type(Ship),
                             planet=PlanetId,
                             created=Created,
                             finish=Finish}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_doc(ship_order()) -> map().
to_doc(SOrder) ->
    Doc = #{<<"ship">> => erlang:atom_to_binary(SOrder#ship_order.ship, utf8),
            <<"planet">> => SOrder#ship_order.planet,
            <<"created">> => SOrder#ship_order.created,
            <<"finish">> => SOrder#ship_order.finish},

    ogonek_util:with_id(SOrder#ship_order.id, Doc).


-spec to_json(ship_order()) -> tuple().
to_json(SOrder) ->
    to_json(SOrder, true).


-spec to_json(ship_order(), boolean()) -> tuple().
to_json(SOrder, _Db) ->
    Values = [{<<"ship">>, SOrder#ship_order.ship},
              {<<"planet">>, SOrder#ship_order.planet},
              {<<"created">>, SOrder#ship_order.created},
              {<<"finish">>, SOrder#ship_order.finish}
             ]
    ++ ogonek_util:if_defined(<<"_id">>, SOrder#ship_order.id),

    ogonek_util:doc(<<"s_order">>, Values).
