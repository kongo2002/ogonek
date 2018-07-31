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
         has_customer_id/1]).


-spec from_json(any()) -> {ok, session()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"ip">>, <<"created">>, <<"updated">>,
            {<<"headers">>, []},
            {<<"customer_id">>, undefined}
           ],

    case ogonek_util:keys(Keys, Json) of
        [Ip, Created, Updated, Headers, CustomerId] ->
            {ok, #session{ip=Ip,
                          created=Created,
                          updated=Updated,
                          headers=Headers,
                          customer_id=CustomerId}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_json(session()) -> tuple().
to_json(#session{}=Session) ->
    Values = [{<<"ip">>, Session#session.ip},
              {<<"created">>, Session#session.created},
              {<<"updated">>, Session#session.updated},
              {<<"headers">>, Session#session.headers}
             ] ++ to_customer_id(Session),

    ogonek_util:doc(<<"session">>, Values).


-spec has_customer_id(session()) -> boolean().
has_customer_id(#session{customer_id=undefined}) -> false;
has_customer_id(#session{customer_id=_CustomerId}) -> true.


to_customer_id(#session{customer_id=undefined}) -> [];
to_customer_id(#session{customer_id=CustomerId}) ->
    [{<<"customer_id">>, CustomerId}].
