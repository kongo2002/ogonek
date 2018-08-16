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

-module(ogonek_auth_local).

-include("ogonek.hrl").

%% Auth behavior
-export([get_info/1,
         auth_user/3,
         validate_login/2]).


-spec get_info(pid()) -> ok.
get_info(_Socket) ->
    % the local auth provider has no auth_info at all
    ok.


-spec auth_user(binary(), binary(), binary()) ->
    {ok, user()} |
    {error, invalid} |
    {error, missing_id} |
    {error, missing_rev} |
    {error, authorization_failed}.
auth_user(Code, Scope, StateStr) ->
    lager:debug("local - trying to authorize with: [code ~p; scope ~p; state ~p]", [Code, Scope, StateStr]),

    % TODO: implement local auth
    {error, invalid}.


-spec validate_login(binary(), user()) -> {ok, user} | error.
validate_login(_SessionId, _User) ->
    % TODO: implement local auth
    error.
