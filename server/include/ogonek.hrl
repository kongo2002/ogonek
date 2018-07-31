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

-define(MSG_TYPE, <<"t">>).

-type kvalue() :: {binary(), any()}.


-record(session, {
          ip :: binary(),
          created :: binary(),
          updated :: binary(),
          headers :: [kvalue()],
          user_id :: binary() | undefined
         }).

-type session() :: #session{}.


-record(oauth_access, {
          access_token :: binary(),
          id_token :: binary(),
          refresh_token :: binary(),
          scope :: [binary()],
          token_type :: binary()
         }).

-type oauth_access() :: #oauth_access{}.


-record(twitch_user, {
          id :: binary(),
          display_name :: binary(),
          email :: binary(),
          profile_image_url :: binary()
         }).

-type twitch_user() :: #twitch_user{}.


-record(user, {
          id :: binary(),
          provider :: binary(),
          provider_id :: binary(),
          email :: binary(),
          name :: binary(),
          img :: binary(),
          oauth :: oauth_access() | undefined
         }).

-type user() :: #user{}.
