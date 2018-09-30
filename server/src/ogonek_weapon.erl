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

-module(ogonek_weapon).

-include("ogonek.hrl").

-export([from_json/1,
         from_doc/1,
         to_doc/1,
         to_json/1,
         to_json/2]).


-spec from_json(json_doc()) -> {ok, weapon()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"_id">>, <<"planet">>, <<"type">>, <<"count">>],

    case ogonek_util:keys(Keys, Json) of
        [Id, Planet, Type, Count] ->
            {ok, #weapon{id=Id,
                         planet=Planet,
                         type=ogonek_weapons:to_weapon_type(Type),
                         count=Count}};
        _Otherwise ->
            {error, invalid}
    end.


-spec from_doc(Doc :: map()) -> {ok, weapon()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"planet">> := PlanetId,
          <<"type">> := Type,
          <<"count">> := Count} ->
            {ok, #weapon{id=maps:get(<<"_id">>, Doc, undefined),
                         planet=PlanetId,
                         type=ogonek_weapons:to_weapon_type(Type),
                         count=Count}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_doc(weapon()) -> map().
to_doc(Weapon) ->
    Doc = #{<<"planet">> => Weapon#weapon.planet,
            <<"type">> => Weapon#weapon.type,
            <<"count">> => Weapon#weapon.count},

    ogonek_util:with_id(Weapon#weapon.id, Doc).


-spec to_json(weapon()) -> tuple().
to_json(Weapon) ->
    to_json(Weapon, true).


-spec to_json(weapon(), boolean()) -> tuple().
to_json(Weapon, Db) ->
    Values = [{<<"count">>, Weapon#weapon.count},
              {<<"planet">>, Weapon#weapon.planet}
             ],

    Values0 =
    case Db of
        true ->
            [{<<"type">>, Weapon#weapon.type} | Values]
            ++ ogonek_util:if_defined(<<"_id">>, Weapon#weapon.id);
        false ->
            WDef = ogonek_weapons:get_definition(Weapon#weapon.type),

            % we recycle the 'db-mode' of ogonek_weapons:to_json in here
            % to omit the 't' tag in the generated json properties
            {Defs} = ogonek_weapons:to_json(WDef, true),

            Values ++ Defs
    end,

    ogonek_util:doc(<<"weapon">>, Values0).

