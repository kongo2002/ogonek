
-record(oauth_access, {
          access_token :: binary(),
          id_token :: binary(),
          refresh_token :: binary(),
          scope :: [binary()],
          token_type :: binary()
         }).

-type oauth_access() :: #oauth_access{}.
