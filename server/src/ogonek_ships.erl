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

-module(ogonek_ships).

-include("include/ogonek.hrl").

-export([definitions/0,
         definitions_map/0,
         get_definition/1,
         to_ship_type/1,
         try_ship_type/1]).


-spec definitions() -> [sdef()].
definitions() ->
    case application:get_env(ships) of
        undefined -> [];
        {ok, Ships} -> Ships
    end.


-spec definitions_map() -> #{atom() => sdef()}.
definitions_map() ->
    lists:foldl(fun(#sdef{name=Name}=SDef, Ss) ->
                        maps:put(Name, SDef, Ss)
                end, maps:new(), definitions()).


-spec get_definition(atom()) -> sdef() | error.
get_definition(Name) ->
    get_definition(Name, definitions()).


-spec get_definition(atom(), [sdef()]) -> sdef() | error.
get_definition(_Name, []) -> error;
get_definition(Name, [#sdef{name=Name}=Def | _Ds]) -> Def;
get_definition(Name, [_ | Ds]) ->
    get_definition(Name, Ds).


-spec to_ship_type(binary()) -> atom().
to_ship_type(TypeName) when is_binary(TypeName) ->
    % this looks scary but the valid list of building types
    % should be already existing via configuration initialization
    erlang:binary_to_existing_atom(TypeName, utf8).


-spec try_ship_type(binary()) -> sdef() | error | {error, invalid}.
try_ship_type(Type) when is_binary(Type) ->
    try
        get_definition(to_ship_type(Type))
    catch
        _Error -> {error, invalid}
    end;

try_ship_type(_Type) ->
    {error, invalid}.
