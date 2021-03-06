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

-module(ogonek_building).

-include("include/ogonek.hrl").

-export([from_json/1,
         to_doc/1,
         from_doc/1,
         to_json/1,
         to_json/2]).


-spec from_json(json_doc()) -> {ok, building()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"_id">>, <<"planet">>, <<"type">>, <<"level">>, <<"created">>],

    case ogonek_util:keys(Keys, Json) of
        [Id, Planet, Type, Level, Created] ->
            {ok, #building{id=Id,
                           planet=Planet,
                           type=ogonek_buildings:to_building_type(Type),
                           level=Level,
                           created=Created}};
        _Otherwise ->
            {error, invalid}
    end.


-spec from_doc(map()) -> {ok, building()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"planet">> := Planet,
          <<"type">> := Type,
          <<"level">> := Level,
          <<"created">> := Created} ->
            {ok, #building{id=ogonek_mongo:from_id(maps:get(<<"_id">>, Doc, undefined)),
                           planet=ogonek_mongo:from_id(Planet),
                           type=ogonek_buildings:to_building_type(Type),
                           level=Level,
                           created=Created}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_doc(building()) -> map().
to_doc(Building) ->
    Doc = #{<<"planet">> => ogonek_mongo:to_id(Building#building.planet),
            <<"type">> => erlang:atom_to_binary(Building#building.type, utf8),
            <<"level">> => Building#building.level,
            <<"created">> => Building#building.created},

    ogonek_util:with_id(Building#building.id, Doc).


-spec to_json(building()) -> tuple().
to_json(Building) ->
    to_json(Building, true).


-spec to_json(building(), boolean()) -> tuple().
to_json(Building, Db) ->
    Values = [{<<"planet">>, Building#building.planet},
              {<<"type">>, Building#building.type},
              {<<"level">>, Building#building.level},
              {<<"created">>, ogonek_util:unixtime_to_millis(Building#building.created)}
             ]
    ++ ogonek_util:if_defined(<<"_id">>, Building#building.id),

    Vs = case Db of
             true -> [];
             false ->
                 % add additional meta information in case the json
                 % is targetted towards the client/non-db
                 Def = ogonek_buildings:calculate_building_costs(Building),
                 Duration = ogonek_buildings:calculate_construction_duration(Building),
                 [{<<"duration">>, Duration} | json_from_definition(Def)]
         end,

    ogonek_util:doc(<<"building">>, Values ++ Vs).


-spec json_from_definition(bdef() | error) -> [{binary(), integer()}].
json_from_definition(error) -> [];
json_from_definition(Def) ->
    [{<<"workers">>, Def#bdef.workers},
     {<<"power">>, Def#bdef.power},
     {<<"iron_ore">>, Def#bdef.iron_ore},
     {<<"gold">>, Def#bdef.gold},
     {<<"h2o">>, Def#bdef.h2o},
     {<<"oil">>, Def#bdef.oil},
     {<<"h2">>, Def#bdef.h2},
     {<<"uranium">>, Def#bdef.uranium},
     {<<"pvc">>, Def#bdef.pvc},
     {<<"titan">>, Def#bdef.titan},
     {<<"kyanite">>, Def#bdef.kyanite},
     {<<"group">>, Def#bdef.group}
    ].
