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

-module(ogonek_weapon_order).

-include("ogonek.hrl").

-export([from_json/1,
         from_doc/1,
         to_doc/1,
         to_json/1,
         to_json/2]).


-spec from_json(json_doc()) -> {ok, weapon_order()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"_id">>, <<"weapon">>, <<"planet">>, <<"created">>, <<"finish">>],

    case ogonek_util:keys(Keys, Json) of
        [Id, Weapon, Planet, Created, Finish] ->
            {ok, #weapon_order{id=Id,
                               weapon=ogonek_weapons:to_weapon_type(Weapon),
                               planet=Planet,
                               created=Created,
                               finish=Finish}};
        _Otherwise ->
            {error, invalid}
    end.


-spec from_doc(Doc :: map()) -> {ok, weapon_order()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"weapon">> := Weapon,
          <<"planet">> := PlanetId,
          <<"created">> := Created,
          <<"finish">> := Finish} ->
            {ok, #weapon_order{id=maps:get(<<"_id">>, Doc, undefined),
                               weapon=ogonek_weapons:to_weapon_type(Weapon),
                               planet=PlanetId,
                               created=Created,
                               finish=Finish}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_doc(weapon_order()) -> map().
to_doc(WOrder) ->
    Doc = #{<<"weapon">> => WOrder#weapon_order.weapon,
            <<"planet">> => WOrder#weapon_order.planet,
            <<"created">> => WOrder#weapon_order.created,
            <<"finish">> => WOrder#weapon_order.finish},

    ogonek_util:with_id(WOrder#weapon_order.id, Doc).


-spec to_json(weapon_order()) -> tuple().
to_json(WOrder) ->
    to_json(WOrder, true).


-spec to_json(weapon_order(), boolean()) -> tuple().
to_json(WOrder, _Db) ->
    Values = [{<<"weapon">>, WOrder#weapon_order.weapon},
              {<<"planet">>, WOrder#weapon_order.planet},
              {<<"created">>, WOrder#weapon_order.created},
              {<<"finish">>, WOrder#weapon_order.finish}
             ]
    ++ ogonek_util:if_defined(<<"_id">>, WOrder#weapon_order.id),

    ogonek_util:doc(<<"w_order">>, Values).
