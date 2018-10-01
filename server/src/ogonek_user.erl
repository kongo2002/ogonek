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
         from_doc/1,
         to_doc/1,
         to_json/1,
         to_json/2,
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


-spec from_doc(map()) -> {ok, user()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"_id">> := Id,
          <<"provider">> := Provider,
          <<"pid">> := Pid,
          <<"email">> := Email,
          <<"name">> := Name,
          <<"img">> := Img} ->
            OAuth = oauth_doc(maps:get(<<"oauth">>, Doc, undefined)),
            {ok, #user{id=ogonek_mongo:from_id(Id),
                       provider=Provider,
                       provider_id=Pid,
                       email=Email,
                       name=Name,
                       img=Img,
                       oauth=OAuth}};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_doc(user()) -> map().
to_doc(User) ->
    Doc = #{<<"_id">> => ogonek_mongo:to_id(User#user.id),
            <<"provider">> => User#user.provider,
            <<"pid">> => User#user.provider_id,
            <<"email">> => User#user.email,
            <<"name">> => User#user.name,
            <<"img">> => User#user.img},

    ogonek_util:with(<<"oauth">>, User#user.oauth, fun ogonek_oauth:to_doc/1, Doc).


-spec to_json(user()) -> tuple().
to_json(User) ->
    to_json(User, true).


-spec to_json(user(), boolean()) -> tuple().
to_json(User, Db) ->
    Values = [{<<"_id">>, User#user.id},
              {<<"provider">>, User#user.provider},
              {<<"pid">>, User#user.provider_id},
              {<<"email">>, User#user.email},
              {<<"name">>, User#user.name},
              {<<"img">>, User#user.img}
             ],

    Values0 = case Db of
                  true -> Values ++ to_oauth(User);
                  false -> Values
              end,

    ogonek_util:doc(<<"user">>, Values0).


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


-spec oauth_doc(map() | undefined) -> oauth_access() | undefined.
oauth_doc(undefined) -> undefined;
oauth_doc(Doc) ->
    case ogonek_oauth:from_doc(Doc) of
        {ok, OAuth} -> OAuth;
        _Otherwise -> undefined
    end.


-spec to_oauth(user()) -> list().
to_oauth(#user{oauth=undefined}) -> [];
to_oauth(#user{oauth=OAuth}) ->
    [{<<"oauth">>, ogonek_oauth:to_json(OAuth)}].
