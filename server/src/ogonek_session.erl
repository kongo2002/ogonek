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

-include("ogonek.hrl").

-export([from_json/1,
         to_json/1,
         has_user_id/1]).


-spec from_json(any()) -> {ok, session()} | {error, invalid}.
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


-spec to_json(session()) -> tuple().
to_json(#session{}=Session) ->
    Values = [{<<"ip">>, Session#session.ip},
              {<<"created">>, Session#session.created},
              {<<"updated">>, Session#session.updated},
              {<<"headers">>, {Session#session.headers}}
             ]
    ++ ogonek_util:if_defined(<<"user_id">>, Session#session.user_id)
    ++ ogonek_util:if_defined(<<"_id">>, Session#session.id),

    ogonek_util:doc(<<"session">>, Values).


-spec has_user_id(session()) -> boolean().
has_user_id(#session{user_id=undefined}) -> false;
has_user_id(#session{user_id=_UserId}) -> true.
