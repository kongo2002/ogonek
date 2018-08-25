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

-module(ogonek_research).

-include("ogonek.hrl").

-export([from_json/1,
         to_json/1,
         to_json/2]).

-export([all_researches/0,
         possible_research/1,
         possible_research/2]).


-spec all_researches() -> [rdef()].
all_researches() ->
    case application:get_env(research) of
        undefined -> [];
        {ok, Research} -> Research
    end.


-spec possible_research([research()]) -> [atom()].
possible_research(Research) ->
    possible_research(Research, all_researches()).


-spec possible_research([research()], [rdef()]) -> [atom()].
possible_research(Research, Definitions) ->
    lists:filter(fun(R) -> research_available(Research, R) end, Definitions).


-spec research_available([research()], rdef()) -> boolean().
research_available(_Research, #rdef{requirements=[]}) -> true;
research_available(Research, #rdef{requirements=Reqs}) ->
    lists:all(fun(Req) -> has_requirement(Research, Req) end, Reqs).


-spec has_requirement([research()], {atom(), integer()}) -> boolean().
has_requirement([], _Requirement) -> false;
has_requirement([Research | Rs], {Name, MinLevel}=Req) ->
    if Research#research.research == Name andalso Research#research.level >= MinLevel ->
           true;
       true ->
           has_requirement(Rs, Req)
    end.


-spec from_json(json_doc()) -> {ok, research()} | {error, invalid}.
from_json(UserJson) ->
    Keys = [<<"_id">>, <<"user">>, <<"research">>, <<"level">>, <<"created">>, <<"finish">>],

    case ogonek_util:keys(Keys, UserJson) of
        [Id, User, Research, Level, Created, Finish] ->
            {ok, #research{id=Id,
                           user=User,
                           research=to_research(Research),
                           level=Level,
                           created=Created,
                           finish=Finish}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_json(research()) -> tuple().
to_json(Research) ->
    to_json(Research, true).


-spec to_json(research(), boolean()) -> tuple().
to_json(Research, _Db) ->
    Values = [{<<"_id">>, Research#research.id},
              {<<"user">>, Research#research.user},
              {<<"research">>, Research#research.research},
              {<<"level">>, Research#research.level},
              {<<"created">>, Research#research.created},
              {<<"finish">>, Research#research.finish}
             ],
    ogonek_util:doc(<<"research">>, Values).


-spec to_research(binary()) -> atom().
to_research(TypeName) when is_binary(TypeName) ->
    % this looks scary but the valid list of building types
    % should be already existing via configuration initialization
    erlang:binary_to_existing_atom(TypeName, utf8).
