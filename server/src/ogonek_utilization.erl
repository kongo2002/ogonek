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

-module(ogonek_utilization).

-include("include/ogonek.hrl").

-export([validate/3,
         from_json/1,
         to_json/1,
         to_json/2]).


-spec from_json(json_doc()) -> {ok, resources()} | {error, invalid}.
from_json(Resources) ->
    ogonek_resources:from_json(Resources).


-spec to_json(resources()) -> tuple().
to_json(Resources) ->
    to_json(Resources, true).


-spec to_json(resources(), boolean()) -> tuple().
to_json(Resources, _Db) ->
    % we are going to reuse the resource's json transformation
    % in database-mode in order to omit the type field
    {Res} = ogonek_resources:to_json(Resources, true),
    WithId = Res ++ ogonek_util:if_defined(<<"planet">>, Resources#resources.planet),

    ogonek_util:doc(<<"utilization">>, WithId).


-spec validate(Utilization :: resources(), Resource :: binary(), integer()) -> {ok, resources()} | error | skipped.
validate(Utilization, Resource, Value) when Value >= 0 andalso Value =< 100 ->
    case Resource of
        <<"pvc">> when Value == Utilization#resources.pvc ->
            skipped;
        <<"pvc">> ->
            {ok, Utilization#resources{pvc=Value}};
        <<"titan">> when Value == Utilization#resources.titan ->
            skipped;
        <<"titan">> ->
            {ok, Utilization#resources{titan=Value}};
        <<"h2">> when Value == Utilization#resources.h2 ->
            skipped;
        <<"h2">> ->
            {ok, Utilization#resources{h2=Value}};
        _Otherwise ->
            error
    end;

validate(_Utilization, _Resource, _Value) -> error.
