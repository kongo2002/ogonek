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
         from_doc/1,
         to_doc/1,
         to_json/1,
         to_json/2]).

-export([all_researches/0,
         has_requirement/2,
         has_requirements/2,
         progress/1,
         research_duration/1,
         possible_research/1,
         possible_research/2,
         research_info_json/3]).


-spec all_researches() -> [rdef()].
all_researches() ->
    case application:get_env(research) of
        undefined -> [];
        {ok, Research} -> Research
    end.


-spec possible_research([research()]) -> [rdef()].
possible_research(Research) ->
    possible_research(Research, all_researches()).


-spec possible_research([research()], [rdef()]) -> [rdef()].
possible_research(Research, Definitions) ->
    lists:filter(fun(R) -> research_available(Research, R) end, Definitions).


-spec research_available([research()], rdef()) -> boolean().
research_available(_Research, #rdef{requirements=[]}) -> true;
research_available(Research, #rdef{requirements=Reqs}) ->
    lists:all(fun(Req) -> has_requirement(Research, Req) end, Reqs).


-spec has_requirement([research()], requirement()) -> boolean().
has_requirement(_, {building, _, _}) -> true;
has_requirement([], _Requirement) -> false;
has_requirement([Research | Rs], {research, Name, MinLevel}=Req) ->
    if Research#research.research == Name andalso Research#research.level >= MinLevel ->
           true;
       true ->
           has_requirement(Rs, Req)
    end.


-spec research_duration([building()]) -> integer() | undefined.
research_duration(Buildings) ->
    % TODO: proper research duration distribution
    ResearchLab = ogonek_buildings:get_building_max_level(Buildings, research_lab),
    ResearchFacility = ogonek_buildings:get_building_max_level(Buildings, research_facility),
    CombinedLevel = ResearchLab + ResearchFacility * 2,

    if CombinedLevel > 0 ->
           % for now we simply limit the duration to 2h at minimum
           % usually the formula should take care of that
           round(max(2, 8 - math:pow(CombinedLevel, 0.3)) * 3600);
       true ->
           undefined
    end.


-spec has_requirements([research()], [requirement()]) -> boolean().
has_requirements(Research, Requirements) ->
    lists:all(fun(Req) -> has_requirement(Research, Req) end, Requirements).


-spec from_json(json_doc()) -> {ok, research()} | {error, invalid}.
from_json(UserJson) ->
    Keys = [<<"_id">>, <<"user">>, <<"research">>, <<"level">>, <<"created">>, <<"finish">>, <<"progress">>],

    case ogonek_util:keys(Keys, UserJson) of
        [Id, User, Research, Level, Created, Finish, Progress] ->
            {ok, #research{id=Id,
                           user=User,
                           research=to_research(Research),
                           level=Level,
                           created=Created,
                           finish=Finish,
                           progress=Progress}};
        _Otherwise ->
            {error, invalid}
    end.


-spec from_doc(Doc :: map()) -> {ok, research()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"_id">> := Id,
          <<"user">> := User,
          <<"research">> := Research,
          <<"level">> := Level,
          <<"created">> := Created,
          <<"finish">> := Finish,
          <<"progress">> := Progress} ->
            {ok, #research{id=ogonek_mongo:from_id(Id),
                           user=ogonek_mongo:from_id(User),
                           research=to_research(Research),
                           level=Level,
                           created=Created,
                           finish=Finish,
                           progress=Progress}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_doc(research()) -> map().
to_doc(Research) ->
    Doc = #{<<"user">> => ogonek_mongo:to_id(Research#research.user),
            <<"research">> => erlang:atom_to_binary(Research#research.research, utf8),
            <<"level">> => Research#research.level,
            <<"created">> => Research#research.created,
            <<"finish">> => Research#research.finish,
            <<"progress">> => Research#research.progress},

    ogonek_util:with_id(Research#research.id, Doc).


-spec to_json(research()) -> tuple().
to_json(Research) ->
    to_json(Research, true).


-spec to_json(research(), boolean()) -> tuple().
to_json(Research, _Db) ->
    Values = [{<<"user">>, Research#research.user},
              {<<"research">>, Research#research.research},
              {<<"level">>, Research#research.level},
              {<<"created">>, Research#research.created},
              {<<"finish">>, Research#research.finish},
              {<<"progress">>, Research#research.progress}
             ]
    ++ ogonek_util:if_defined(<<"_id">>, Research#research.id),

    ogonek_util:doc(<<"research">>, Values).


-spec research_info_json(research() | undefined, [research()], integer() | undefined) -> json_doc().
research_info_json(Running, Finished, Duration) ->
    Status = case Running of
                 undefined -> [];
                 _IsRunning ->
                     Finish = Running#research.finish,
                     Created = Running#research.created,
                     Progress = progress(Created, Finish),
                     InProgress = if Progress >= 50 -> [{<<"name">>, Running#research.research}];
                                     true -> []
                                  end,

                     Status0 = {[{<<"finish">>, Finish},
                                 {<<"created">>, Created}
                                ] ++ InProgress},
                     [{<<"status">>, Status0}]
             end,
    Sorted = lists:keysort(4, Finished),
    Research = lists:map(fun(R) ->
                                 {[{<<"name">>, R#research.research},
                                   {<<"level">>, R#research.level}]}
                         end, Sorted),

    ogonek_util:doc(<<"research">>,
                    [{<<"research">>, Research},
                     {<<"duration">>, Duration}
                    ] ++ Status).


-spec to_research(binary()) -> atom().
to_research(TypeName) when is_binary(TypeName) ->
    % this looks scary but the valid list of building types
    % should be already existing via configuration initialization
    erlang:binary_to_existing_atom(TypeName, utf8).


-spec progress(research()) -> integer().
progress(#research{created=Started, finish=Finished}) ->
    progress(Started, Finished).


-spec progress(timestamp(), timestamp()) -> integer().
progress(Started, Finished) ->
    Total = ogonek_util:seconds_since(Started, Finished),
    Progress = ogonek_util:seconds_since(Started),
    (Progress * 100) div Total.
