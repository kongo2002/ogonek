
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
