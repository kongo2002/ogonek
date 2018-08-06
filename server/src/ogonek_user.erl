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
         to_json/1,
         has_oauth/1]).


-spec from_json(json_doc()) -> {ok, user()} | {error, invalid}.
from_json(UserJson) ->
    Keys = [<<"_id">>, <<"provider">>, <<"pid">>, <<"email">>, <<"name">>, <<"img">>,
            {<<"oauth">>, undefined} % oauth is optional
           ],

    case ogonek_util:keys(Keys, UserJson) of
        [Id, Provider, Pid, Email, Name, Img, OAuth] ->
            {ok, #user{id=Id,
                       provider=Provider,
                       provider_id=Pid,
                       email=Email,
                       name=Name,
                       img=Img,
                       oauth=from_oauth(OAuth)}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_json(user()) -> tuple().
to_json(User) ->
    Values = [{<<"_id">>, User#user.id},
              {<<"provider">>, User#user.provider},
              {<<"pid">>, User#user.provider_id},
              {<<"email">>, User#user.email},
              {<<"name">>, User#user.name},
              {<<"img">>, User#user.img}
             ] ++ to_oauth(User),
    ogonek_util:doc(<<"user">>, Values).


-spec has_oauth(user()) -> boolean().
has_oauth(#user{oauth=undefined}) -> false;
has_oauth(#user{oauth=_OAuth}) -> true.


-spec from_oauth(json_doc() | undefined) -> oauth_access() | undefined.
from_oauth(undefined) -> undefined;
from_oauth(Json) ->
    case ogonek_oauth:from_json(Json) of
        {ok, OAuth} -> OAuth;
        _Otherwise -> undefined
    end.


-spec to_oauth(user()) -> list().
to_oauth(#user{oauth=undefined}) -> [];
to_oauth(#user{oauth=OAuth}) ->
    [{<<"oauth">>, ogonek_oauth:to_json(OAuth)}].
