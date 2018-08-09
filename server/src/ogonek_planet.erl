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

-module(ogonek_planet).

-include("ogonek.hrl").

-export([from_json/1,
         to_json/1,
         to_json/2,
         exists/1,
         exists/3]).


-spec from_json(json_doc()) -> {ok, planet()} | {error, invalid}.
from_json(Planet) ->
    Keys = [<<"_id">>, <<"type">>, <<"size">>, <<"pos">>, <<"idx">>,
            {<<"owner">>, undefined}
           ],

    case ogonek_util:keys(Keys, Planet) of
        [Id, Type, Size, [X, Y, Z], Idx, Owner] ->
            case parse_type(Type) of
                error -> {error, invalid};
                Type0 ->
                    {ok, #planet{id=Id,
                                 type=Type0,
                                 size=Size,
                                 position={X, Y, Z},
                                 index=Idx,
                                 owner=Owner}}
            end;
        _Otherwise ->
            {error, invalid}
    end.


-spec to_json(planet()) -> tuple().
to_json(Planet) ->
    to_json(Planet, true).


-spec to_json(planet(), boolean()) -> tuple().
to_json(Planet, _Db) ->
    {X, Y, Z} = Planet#planet.position,
    Values = [{<<"type">>, Planet#planet.type},
              {<<"size">>, Planet#planet.size},
              {<<"pos">>, [X, Y, Z]},
              {<<"idx">>, Planet#planet.index}
             ]
    ++ ogonek_util:if_defined(<<"_id">>, Planet#planet.id)
    ++ ogonek_util:if_defined(<<"owner">>, Planet#planet.owner),
    ogonek_util:doc(<<"planet">>, Values).


-spec exists(coordinate()) -> boolean().
exists({X, Y, Z}) ->
    exists(X, Y, Z).


-spec exists(integer(), integer(), integer()) -> boolean().
exists(X, Y, Z) ->
    ogonek_db:planet_exists(X, Y, Z).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_type(binary()) -> planet_type() | error.
parse_type(<<"earth">>) -> earth;
parse_type(<<"water">>) -> water;
parse_type(<<"fire">>) -> fire;
parse_type(<<"ice">>) -> ice;
parse_type(_Invalid) -> error.
