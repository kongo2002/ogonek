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

-include("ogonek.hrl").

-export([from_json/1,
         to_json/1]).


-spec from_json(term()) -> {ok, building()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"_id">>, <<"planet">>, <<"type">>, <<"level">>, <<"created">>],

    case ogonek_util:keys(Keys, Json) of
        [Id, Planet, Type, Level, Created] ->
            {ok, #building{id=Id,
                           planet=Planet,
                           type=to_building_type(Type),
                           level=Level,
                           created=Created}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_json(building()) -> tuple().
to_json(Building) ->
    Values = [{<<"planet">>, Building#building.id},
              {<<"type">>, Building#building.type},
              {<<"level">>, Building#building.level},
              {<<"created">>, Building#building.created}
             ]
    ++ ogonek_util:if_defined(<<"_id">>, Building#building.id),

    ogonek_util:doc(<<"building">>, Values).


-spec to_building_type(binary()) -> atom().
to_building_type(TypeName) when is_binary(TypeName) ->
    % this looks scary but the valid list of building types
    % should be already existing via configuration initialization
    erlang:binary_to_existing_atom(TypeName, utf8).
