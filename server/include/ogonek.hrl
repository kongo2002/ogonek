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

-type json_props() :: [tuple()].

-type json_doc() :: json_props() | {json_props()}.

-type kvalue() :: {binary(), any()}.

-type maybe_unset_id() :: binary() | undefined.

-type timestamp() :: binary().


-record(session, {
          id :: maybe_unset_id(),
          ip :: binary(),
          created :: timestamp(),
          updated :: timestamp(),
          headers :: [kvalue()],
          user_id :: maybe_unset_id()
         }).

-type session() :: #session{}.

-type auth_provider() :: twitch | local.

-type auth_provider_module() :: ogonek_twitch | ogonek_auth_local.


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


-record(ws_state, {
          session_id :: binary(),
          user_id :: maybe_unset_id()
         }).

-type ws_state() :: #ws_state{}.


-record(resources, {
          planet :: maybe_unset_id(),
          workers :: integer(),
          power :: integer(),
          iron_ore :: integer(),
          gold :: integer(),
          h2o :: integer(),
          oil :: integer() ,
          h2 :: integer(),
          uranium :: integer(),
          pvc :: integer(),
          titan :: integer(),
          kyanite :: integer(),
          updated :: timestamp() | undefined
         }).

-type resources() :: #resources{}.


-record(capacity, {
          planet :: maybe_unset_id(),
          iron_ore :: integer(),
          gold :: integer(),
          h2o :: integer(),
          oil :: integer() ,
          h2 :: integer(),
          uranium :: integer(),
          pvc :: integer(),
          titan :: integer(),
          kyanite :: integer()
         }).

-type capacity() :: #capacity{}.


-type coordinate() :: {integer(), integer(), integer()}.


-type planet_type() :: earth | water | fire | ice.


-record(planet, {
          id :: maybe_unset_id(),
          type :: planet_type(),
          size :: integer(),
          position :: coordinate(),
          index :: integer(),
          owner :: maybe_unset_id(),
          resources :: resources(),
          utilization :: resources()
         }).

-type planet() :: #planet{}.


-type requirement_type() :: building | research.

-type requirement() :: {requirement_type(), atom(), integer()}.


-record(bdef, {
          name :: atom(),
          workers :: integer(),
          power :: integer(),
          iron_ore :: integer(),
          gold :: integer(),
          h2o :: integer(),
          oil :: integer() ,
          h2 :: integer(),
          uranium :: integer(),
          pvc :: integer(),
          titan :: integer(),
          kyanite :: integer(),
          group :: atom(),
          requirements :: [requirement()]
         }).

-type bdef() :: #bdef{}.


-record(building, {
          id :: maybe_unset_id(),
          planet :: binary(),
          type :: atom(),
          level :: integer(),
          created :: timestamp()
         }).

-type building() :: #building{}.


-record(construction, {
          id :: maybe_unset_id(),
          building :: atom(),
          level :: integer(),
          planet :: binary(),
          created :: timestamp(),
          finish :: timestamp()
         }).

-type construction() :: #construction{}.


-record(rdef, {
          name :: atom(),
          requirements :: [requirement()]
    }).

-type rdef() :: #rdef{}.


-record(research, {
          id :: maybe_unset_id(),
          user :: binary(),
          research :: atom(),
          level :: integer(),
          created :: timestamp(),
          finish :: timestamp(),
          progress :: boolean()
         }).

-type research() :: #research{}.


-record(wdef, {
          name :: atom(),
          duration :: non_neg_integer(),
          space :: non_neg_integer(),
          power :: non_neg_integer(),
          damage :: float(),
          load :: non_neg_integer(),
          iron_ore :: integer(),
          gold :: integer(),
          h2o :: integer(),
          oil :: integer() ,
          h2 :: integer(),
          uranium :: integer(),
          pvc :: integer(),
          titan :: integer(),
          kyanite :: integer()
         }).

-type wdef() :: #wdef{}.


-record(weapon_order, {
          id :: maybe_unset_id(),
          weapon :: atom(),
          planet :: binary(),
          created :: timestamp(),
          finish :: timestamp()
         }).

-type weapon_order() :: #weapon_order{}.
