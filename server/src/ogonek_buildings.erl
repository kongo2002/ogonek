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

-module(ogonek_buildings).

-include("ogonek.hrl").

-export([definitions/0,
         definitions_map/0]).


-spec definitions() -> [bdef()].
definitions() ->
    case application:get_env(buildings) of
        undefined -> [];
        {ok, Buildings} -> Buildings
    end.


-spec definitions_map() -> #{atom() => bdef()}.
definitions_map() ->
    lists:foldl(fun(#bdef{name=Name}=BDef, Bs) ->
                        maps:put(Name, BDef, Bs)
                end, maps:new(), definitions()).
