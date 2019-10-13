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

-module(ogonek_construction).

-include("include/ogonek.hrl").

-export([from_json/1,
         from_doc/1,
         to_doc/1,
         to_json/1,
         to_json/2]).


-spec from_json(json_doc()) -> {ok, construction()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"_id">>, <<"building">>, <<"level">>, <<"planet">>, <<"created">>, <<"finish">>],

    case ogonek_util:keys(Keys, Json) of
        [Id, Building, Level, Planet, Created, Finish] ->
            {ok, #construction{id=Id,
                               building=ogonek_buildings:to_building_type(Building),
                               level=Level,
                               planet=Planet,
                               created=Created,
                               finish=Finish}};
        _Otherwise ->
            {error, invalid}
    end.


-spec from_doc(Doc :: map()) -> {ok, construction()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"building">> := Building,
          <<"level">> := Level,
          <<"planet">> := Planet,
          <<"created">> := Created,
          <<"finish">> := Finish} ->
            {ok, #construction{id=ogonek_mongo:from_id(maps:get(<<"_id">>, Doc, undefined)),
                               building=ogonek_buildings:to_building_type(Building),
                               level=Level,
                               planet=ogonek_mongo:from_id(Planet),
                               created=Created,
                               finish=Finish}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_doc(construction()) -> map().
to_doc(Construction) ->
    Doc = #{<<"building">> => erlang:atom_to_binary(Construction#construction.building, utf8),
            <<"level">> => Construction#construction.level,
            <<"planet">> => ogonek_mongo:to_id(Construction#construction.planet),
            <<"created">> => Construction#construction.created,
            <<"finish">> => Construction#construction.finish},

    ogonek_util:with_id(Construction#construction.id, Doc).


-spec to_json(construction()) -> tuple().
to_json(Construction) ->
    to_json(Construction, true).


-spec to_json(construction(), boolean()) -> tuple().
to_json(Construction, _Db) ->
    Values = [{<<"building">>, Construction#construction.building},
              {<<"level">>, Construction#construction.level},
              {<<"planet">>, Construction#construction.planet},
              {<<"created">>, ogonek_util:unixtime_to_millis(Construction#construction.created)},
              {<<"finish">>, ogonek_util:unixtime_to_millis(Construction#construction.finish)}
             ]
    ++ ogonek_util:if_defined(<<"_id">>, Construction#construction.id),

    ogonek_util:doc(<<"construction">>, Values).
