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

-module(ogonek_session).

-include("include/ogonek.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([from_json/1,
         from_doc/1,
         to_json/1,
         to_json/2,
         has_user_id/1]).


-spec from_json(json_doc()) -> {ok, session()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"_id">>, <<"ip">>, <<"created">>, <<"updated">>,
            {<<"headers">>, []},
            {<<"user_id">>, undefined}
           ],

    case ogonek_util:keys(Keys, Json) of
        [Id, Ip, Created, Updated, Headers, UserId] ->
            {ok, #session{id=Id,
                          ip=Ip,
                          created=Created,
                          updated=Updated,
                          headers=Headers,
                          user_id=UserId}};
        _Otherwise ->
            {error, invalid}
    end.


-spec from_doc(Doc :: map()) -> {ok, session()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"_id">> := Id,
          <<"created">> := Created,
          <<"updated">> := Updated,
          <<"headers">> := Headers,
          <<"ip">> := Ip} ->
            UserId = maps:get(<<"user_id">>, Doc, undefined),
            {ok, #session{id=ogonek_mongo:from_id(Id),
                          ip=Ip,
                          created=Created,
                          updated=Updated,
                          headers=Headers,
                          user_id=ogonek_mongo:from_id(UserId)}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_json(session()) -> tuple().
to_json(Session) ->
    to_json(Session, true).


-spec to_json(session(), boolean()) -> tuple().
to_json(Session, _Db) ->
    Values = [{<<"ip">>, Session#session.ip},
              {<<"created">>, ogonek_util:unixtime_to_millis(Session#session.created)},
              {<<"updated">>, ogonek_util:unixtime_to_millis(Session#session.updated)},
              {<<"headers">>, {Session#session.headers}}
             ]
    ++ ogonek_util:if_defined(<<"user_id">>, Session#session.user_id)
    ++ ogonek_util:if_defined(<<"_id">>, Session#session.id),

    ogonek_util:doc(<<"session">>, Values).


-spec has_user_id(session()) -> boolean().
has_user_id(#session{user_id=undefined}) -> false;
has_user_id(#session{user_id=_UserId}) -> true.


%%
%% TESTS
%%

-ifdef(TEST).

from_doc_test_() ->
    Now = erlang:timestamp(),
    Id = <<"id">>,
    Ip = <<"127.0.0.1">>,

    [?_assertEqual({error, invalid}, from_doc(#{})),
     ?_assertEqual({ok, #session{id=Id,
                                 ip=Ip,
                                 created=Now,
                                 updated=Now,
                                 headers=[]}}, from_doc(#{<<"headers">> => [],
                                                          <<"updated">> => Now,
                                                          <<"created">> => Now,
                                                          <<"ip">> => Ip,
                                                          <<"_id">> => Id
                                                         }))
    ].

-endif.
