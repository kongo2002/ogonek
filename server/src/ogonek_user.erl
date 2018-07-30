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

-module(ogonek_user).

-include("ogonek.hrl").

-export([from_json/1,
         to_json/1]).


-spec from_json(any()) -> {ok, user()} | {error, invalid_user}.
from_json(UserJson) ->
    Keys = [<<"_id">>, <<"provider">>, <<"pid">>, <<"email">>, <<"name">>, <<"img">>],

    case ogonek_util:keys(Keys, UserJson) of
        [Id, Provider, Pid, Email, Name, Img] ->
            {ok, #user{id=Id,
                       provider=Provider,
                       provider_id=Pid,
                       email=Email,
                       name=Name,
                       img=Img}};
        _Otherwise ->
            {error, invalid_user}
    end.


-spec to_json(user()) -> tuple().
to_json(#user{}=User) ->
    ogonek_util:doc(<<"user">>,
                    {[{<<"_id">>, User#user.id},
                      {<<"provider">>, User#user.provider},
                      {<<"pid">>, User#user.provider_id},
                      {<<"email">>, User#user.email},
                      {<<"name">>, User#user.name},
                      {<<"img">>, User#user.img}
                     ]}).
