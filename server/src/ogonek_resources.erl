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
         to_json/2,
         empty/0,
         multiply/2,
         sum/2,
         with_factor/2]).


-spec empty() -> resources().
empty() ->
    #resources{planet=undefined,
               workers=0,
               power=0,
               iron_ore=0,
               gold=0,
               h2o=0,
               oil=0,
               h2=0,
               uranium=0,
               pvc=0,
               kyanite=0}.


-spec from_json(json_doc()) -> {ok, resources()} | {error, invalid}.
from_json(Resources) ->
    Keys = [{<<"workers">>, 0},
            {<<"power">>, 0},
            <<"iron_ore">>, <<"gold">>, <<"h2o">>, <<"oil">>,
            <<"h2">>, <<"uranium">>, <<"pvc">>, <<"kyanite">>,
            {<<"planet">>, undefined},
            {<<"updated">>, undefined}
           ],

    case ogonek_util:keys(Keys, Resources) of
        [Workers, Power, Iron, Gold, H2O, Oil, H2, Uranium, Pvc, Kyanite, Planet, Upd] ->
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
                            kyanite=Kyanite,
                            updated=Upd}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_json(resources()) -> tuple().
to_json(Resources) ->
    to_json(Resources, true).


-spec to_json(resources(), boolean()) -> tuple().
to_json(Resources, Db) ->
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
             ] ++ ogonek_util:if_defined(<<"updated">>, Resources#resources.updated),

    case Db of
        true -> {Values};
        false ->
            % no need to store type and planet in the database
            Vs = Values ++ ogonek_util:if_defined(<<"planet">>, Resources#resources.planet),
            ogonek_util:doc(<<"resources">>, Vs)
    end.


-spec multiply(resources(), resources()) -> resources().
multiply(ResA, ResB) ->
    ResA#resources{
      iron_ore=ResA#resources.iron_ore * ResB#resources.iron_ore,
      gold=ResA#resources.gold * ResB#resources.gold,
      h2o=ResA#resources.h2o * ResB#resources.h2o,
      oil=ResA#resources.oil * ResB#resources.oil,
      h2=ResA#resources.h2 * ResB#resources.h2,
      uranium=ResA#resources.uranium * ResB#resources.uranium,
      pvc=ResA#resources.pvc * ResB#resources.pvc,
      kyanite=ResA#resources.kyanite * ResB#resources.kyanite
     }.


-spec sum(resources(), resources()) -> resources().
sum(ResA, ResB) ->
    ResA#resources{
      iron_ore=ResA#resources.iron_ore + ResB#resources.iron_ore,
      gold=ResA#resources.gold + ResB#resources.gold,
      h2o=ResA#resources.h2o + ResB#resources.h2o,
      oil=ResA#resources.oil + ResB#resources.oil,
      h2=ResA#resources.h2 + ResB#resources.h2,
      uranium=ResA#resources.uranium + ResB#resources.uranium,
      pvc=ResA#resources.pvc + ResB#resources.pvc,
      kyanite=ResA#resources.kyanite + ResB#resources.kyanite
     }.


-spec with_factor(float(), resources()) -> resources().
with_factor(Factor, Resources) ->
    Resources#resources{
      iron_ore=round(Resources#resources.iron_ore * Factor),
      gold=round(Resources#resources.gold * Factor),
      h2o=round(Resources#resources.h2o * Factor),
      oil=round(Resources#resources.oil * Factor),
      h2=round(Resources#resources.h2 * Factor),
      uranium=round(Resources#resources.uranium * Factor),
      pvc=round(Resources#resources.pvc * Factor),
      kyanite=round(Resources#resources.kyanite * Factor)
     }.
