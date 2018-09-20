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
         substract_costs/2,
         with_factor/2,
         with_capacity/2]).


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
               titan=0,
               kyanite=0}.


-spec from_json(json_doc()) -> {ok, resources()} | {error, invalid}.
from_json(Resources) ->
    Keys = [{<<"workers">>, 0},
            {<<"power">>, 0},
            <<"iron_ore">>, <<"gold">>, <<"h2o">>, <<"oil">>,
            <<"h2">>, <<"uranium">>, <<"pvc">>, <<"titan">>, <<"kyanite">>,
            {<<"planet">>, undefined},
            {<<"updated">>, undefined}
           ],

    case ogonek_util:keys(Keys, Resources) of
        [Workers, Power, Iron, Gold, H2O, Oil, H2, Uranium, Pvc, Titan, Kyanite, Planet, Upd] ->
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
                            titan=Titan,
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
              {<<"titan">>, Resources#resources.titan},
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
      titan=ResA#resources.titan * ResB#resources.titan,
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
      titan=ResA#resources.titan + ResB#resources.titan,
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
      titan=round(Resources#resources.titan * Factor),
      kyanite=round(Resources#resources.kyanite * Factor)
     }.


-spec with_capacity(resources(), capacity()) -> resources().
with_capacity(Resources, Capacity) ->
    Resources#resources{
      iron_ore=min(Resources#resources.iron_ore, Capacity#capacity.iron_ore),
      gold=min(Resources#resources.gold, Capacity#capacity.gold),
      h2o=min(Resources#resources.h2o, Capacity#capacity.h2o),
      oil=min(Resources#resources.oil, Capacity#capacity.oil),
      h2=min(Resources#resources.h2, Capacity#capacity.h2),
      uranium=min(Resources#resources.uranium, Capacity#capacity.uranium),
      pvc=min(Resources#resources.pvc, Capacity#capacity.pvc),
      titan=min(Resources#resources.titan, Capacity#capacity.titan),
      kyanite=min(Resources#resources.kyanite, Capacity#capacity.kyanite)
     }.


-spec substract_costs(resources(), Costs :: bdef() | wdef()) -> resources().
substract_costs(Resources, #bdef{}=Costs) ->
    % the 'positive' gain of workers and/or power will not be
    % considered in here but instead once the construction is actually finished
    Workers = max(Costs#bdef.workers, 0),
    Power = max(Costs#bdef.power, 0),

    Resources#resources{
      workers=Resources#resources.workers - Workers,
      power=Resources#resources.power - Power,
      iron_ore=Resources#resources.iron_ore - Costs#bdef.iron_ore,
      gold=Resources#resources.gold - Costs#bdef.gold,
      h2o=Resources#resources.h2o - Costs#bdef.h2o,
      oil=Resources#resources.oil - Costs#bdef.oil,
      h2=Resources#resources.h2 - Costs#bdef.h2,
      uranium=Resources#resources.uranium - Costs#bdef.uranium,
      pvc=Resources#resources.pvc - Costs#bdef.pvc,
      titan=Resources#resources.titan - Costs#bdef.titan,
      kyanite=Resources#resources.kyanite - Costs#bdef.kyanite
     };

substract_costs(Resources, #wdef{}=Costs) ->
    Resources#resources{
      iron_ore=Resources#resources.iron_ore - Costs#wdef.iron_ore,
      gold=Resources#resources.gold - Costs#wdef.gold,
      h2o=Resources#resources.h2o - Costs#wdef.h2o,
      oil=Resources#resources.oil - Costs#wdef.oil,
      h2=Resources#resources.h2 - Costs#wdef.h2,
      uranium=Resources#resources.uranium - Costs#wdef.uranium,
      pvc=Resources#resources.pvc - Costs#wdef.pvc,
      titan=Resources#resources.titan - Costs#wdef.titan,
      kyanite=Resources#resources.kyanite - Costs#wdef.kyanite
     }.
