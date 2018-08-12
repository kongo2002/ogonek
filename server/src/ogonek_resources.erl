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

-module(ogonek_resources).

-include("ogonek.hrl").

-export([from_json/1,
         to_json/1,
         to_json/2]).


-spec from_json(json_doc()) -> {ok, resources()} | {error, invalid}.
from_json(Resources) ->
    Keys = [{<<"workers">>, 0},
            {<<"power">>, 0},
            <<"iron_ore">>, <<"gold">>, <<"h2o">>, <<"oil">>,
            <<"h2">>, <<"uranium">>, <<"pvc">>, <<"kyanite">>,
            {<<"planet">>, undefined}
           ],

    case ogonek_util:keys(Keys, Resources) of
        [Workers, Power, Iron, Gold, H2O, Oil, H2, Uranium, Pvc, Kyanite, Planet] ->
            {ok, #resources{planet=Planet,
                            workers=Workers,
                            power=Power,
                            iron_ore=Iron,
                            gold=Gold,
                            h2o=H2O,
                            oil=Oil,
                            h2=H2,
                            uranium=Uranium,
                            pvc=Pvc,
                            kyanite=Kyanite}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_json(resources()) -> tuple().
to_json(Resources) ->
    to_json(Resources, true).


-spec to_json(resources(), boolean()) -> tuple().
to_json(Resources, _Db) ->
    Values = [{<<"workers">>, Resources#resources.workers},
              {<<"power">>, Resources#resources.power},
              {<<"iron_ore">>, Resources#resources.iron_ore},
              {<<"gold">>, Resources#resources.gold},
              {<<"h2o">>, Resources#resources.h2o},
              {<<"oil">>, Resources#resources.oil},
              {<<"h2">>, Resources#resources.h2},
              {<<"uranium">>, Resources#resources.uranium},
              {<<"pvc">>, Resources#resources.pvc},
              {<<"kyanite">>, Resources#resources.kyanite}
             ]
    ++ ogonek_util:if_defined(<<"planet">>, Resources#resources.planet),

    ogonek_util:doc(<<"resources">>, Values).
