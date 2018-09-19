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
         get_definition/1]).


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
