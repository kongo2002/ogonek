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

-module(ogonek_auth).

-include("ogonek.hrl").

-export([default_provider/0,
         provider_module/1,
         provider_from_binary/1,
         send_auth_infos/1
        ]).


-spec send_auth_infos(pid()) -> ok.
send_auth_infos(Socket) ->
    lists:foreach(fun(Module) ->
                          Module:get_info(Socket)
                  end, all_provider_modules()).


-spec default_provider() -> binary().
default_provider() ->
    <<"twitch">>.


-spec provider_module(auth_provider()) -> auth_provider_module().
provider_module(twitch) -> ogonek_twitch;
provider_module(local) -> ogonek_auth_local;
provider_module(Unknown) ->
    lager:warning("unknown auth provider - fallback to 'local': ~p", [Unknown]),
    ogonek_auth_local.


-spec provider_from_binary(binary()) -> auth_provider() | error.
provider_from_binary(<<"twitch">>) -> twitch;
provider_from_binary(<<"local">>) -> local;
provider_from_binary(_Unknown) -> error.


-spec all_provider_modules() -> [auth_provider_module()].
all_provider_modules() ->
    [ogonek_twitch, ogonek_auth_local].
