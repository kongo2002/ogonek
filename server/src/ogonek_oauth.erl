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

-module(ogonek_oauth).

-include("include/ogonek.hrl").

-export([from_json/1,
         from_doc/1,
         to_doc/1,
         to_json/1,
         to_json/2]).


-spec from_json(json_doc()) -> {ok, oauth_access()} | {error, invalid}.
from_json(Json) ->
    Keys = [<<"atoken">>, <<"id_token">>, <<"rtoken">>, <<"scope">>, <<"ttype">>],

    case ogonek_util:keys(Keys, Json) of
        [AToken, IdToken, RefreshT, Scope, TokenT] ->
            {ok, #oauth_access{access_token=AToken,
                               id_token=IdToken,
                               refresh_token=RefreshT,
                               scope=Scope,
                               token_type=TokenT
                              }};
        _Otherwise -> {error, invalid}
    end.


-spec from_doc(map()) -> {ok, oauth_access()} | {error, invalid}.
from_doc(Doc) ->
    case Doc of
        #{<<"atoken">> := AToken,
          <<"id_token">> := IdToken,
          <<"rtoken">> := RefreshT,
          <<"scope">> := Scope,
          <<"ttype">> := TokenT} ->
            {ok, #oauth_access{access_token=AToken,
                               id_token=IdToken,
                               refresh_token=RefreshT,
                               scope=Scope,
                               token_type=TokenT
                              }};
        _Otherwise ->
            {error, invalid}
    end.


-spec to_doc(oauth_access()) -> map().
to_doc(OAuth) ->
    #{<<"atoken">> => OAuth#oauth_access.access_token,
      <<"id_token">> => OAuth#oauth_access.id_token,
      <<"rtoken">> => OAuth#oauth_access.refresh_token,
      <<"scope">> => OAuth#oauth_access.scope,
      <<"ttype">> => OAuth#oauth_access.token_type}.


-spec to_json(oauth_access()) -> tuple().
to_json(OAuth) ->
    to_json(OAuth, true).


-spec to_json(oauth_access(), boolean()) -> tuple().
to_json(OAuth, _Db) ->
    {[{<<"atoken">>, OAuth#oauth_access.access_token},
      {<<"id_token">>, OAuth#oauth_access.id_token},
      {<<"rtoken">>, OAuth#oauth_access.refresh_token},
      {<<"scope">>, OAuth#oauth_access.scope},
      {<<"ttype">>, OAuth#oauth_access.token_type}
     ]}.
