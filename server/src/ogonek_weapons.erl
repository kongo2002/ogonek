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

-module(ogonek_weapons).

-include("ogonek.hrl").

-export([definitions/0,
         definitions_map/0,
         get_definition/1,
         to_weapon_type/1,
         try_weapon_type/1,
         calculate_order_duration/2,
         to_json/1,
         to_json/2]).


-spec definitions() -> [wdef()].
definitions() ->
    case application:get_env(weapons) of
        undefined -> [];
        {ok, Weapons} -> Weapons
    end.


-spec definitions_map() -> #{atom() => wdef()}.
definitions_map() ->
    lists:foldl(fun(#wdef{name=Name}=WDef, Ws) ->
                        maps:put(Name, WDef, Ws)
                end, maps:new(), definitions()).


-spec get_definition(atom()) -> wdef() | error.
get_definition(Name) ->
    get_definition(Name, definitions()).


-spec get_definition(atom(), [wdef()]) -> wdef() | error.
get_definition(_Name, []) -> error;
get_definition(Name, [#wdef{name=Name}=Def | _Ds]) -> Def;
get_definition(Name, [_ | Ds]) ->
    get_definition(Name, Ds).


-spec to_weapon_type(binary()) -> atom().
to_weapon_type(TypeName) when is_binary(TypeName) ->
    % this looks scary but the valid list of building types
    % should be already existing via configuration initialization
    erlang:binary_to_existing_atom(TypeName, utf8).


-spec try_weapon_type(binary()) -> wdef() | error | {error, invalid}.
try_weapon_type(Type) when is_binary(Type) ->
    try
        get_definition(to_weapon_type(Type))
    catch
        _Error -> {error, invalid}
    end;

try_weapon_type(_Type) ->
    {error, invalid}.


-spec calculate_order_duration([building()], wdef()) -> non_neg_integer().
calculate_order_duration(_Buildings, #wdef{duration=Duration}) ->
    % TODO: proper distribution
    % take level of weapon_manufacture into account
    round(Duration / ?OGONEK_DEFAULT_ACCELERATION).


-spec to_json(wdef()) -> json_doc().
to_json(WDef) ->
    to_json(WDef, true).


-spec to_json(wdef(), boolean()) -> json_doc().
to_json(WDef, Db) ->
    Vs = [{<<"name">>, WDef#wdef.name},
          % stats
          {<<"duration">>, WDef#wdef.duration},
          {<<"space">>, WDef#wdef.space},
          {<<"power">>, WDef#wdef.power},
          {<<"dmg">>, WDef#wdef.damage},
          {<<"load">>, WDef#wdef.load},
          % resources
          {<<"iron_ore">>, WDef#wdef.iron_ore},
          {<<"gold">>, WDef#wdef.gold},
          {<<"h2o">>, WDef#wdef.h2o},
          {<<"oil">>, WDef#wdef.oil},
          {<<"h2">>, WDef#wdef.h2},
          {<<"uranium">>, WDef#wdef.uranium},
          {<<"pvc">>, WDef#wdef.pvc},
          {<<"titan">>, WDef#wdef.titan},
          {<<"kyanite">>, WDef#wdef.kyanite}
         ],

    % as 'wdef' objects won't ever be stored in the database
    % at all we can use the 'db-mode' flag to toggle the
    % type tag 't' in the generated json properties
    case Db of
        true -> {Vs};
        false -> ogonek_util:doc(<<"wdef">>, Vs)
    end.
