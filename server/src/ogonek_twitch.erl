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

-module(ogonek_twitch).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ogonek.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([get_info/0,
         get_auth_token/1,
         get_user/1,
         validate_token/1,
         refresh_token/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {enabled, client_id, client_secret, redirect_uri}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_info() ->
    gen_server:call(?MODULE, get_info).


get_auth_token(AuthCode) ->
    case gen_server:call(?MODULE, {auth_token_request, AuthCode}) of
        {ok, Target} ->
            case ogonek_util:json_post(Target) of
                {ok, 200, _Hs, Body} ->
                    extract_oauth_access(Body);
                Error ->
                    lager:warning("get_auth_token failed: ~p", [Error]),
                    error
            end;
        Error -> Error
    end.


get_user(#oauth_access{access_token=Token}) ->
    Target = <<"https://api.twitch.tv/helix/users">>,
    Headers = [{<<"Accept">>, <<"application/json">>},
               {<<"Authorization">>, <<"Bearer ", Token/binary>>}],

    case ogonek_util:json_get(Target, Headers) of
        {ok, 200, _Hs, Json} ->
            extract_user(Json);
        Error -> Error
    end.


-spec validate_token(binary()) -> {ok, binary()} | error.
validate_token(AccessToken) ->
    case gen_server:call(?MODULE, get_client_id) of
        {ok, ClientId} ->
            Target = <<"https://id.twitch.tv/oauth2/validate">>,
            Headers = [{<<"Authorization">>, <<"OAuth ", AccessToken/binary>>}],
            case ogonek_util:json_get(Target, Headers) of
                {ok, 200, _Hs, Body} ->
                    case ogonek_util:keys([<<"user_id">>, <<"client_id">>], Body) of
                        [UserId, ClientId] -> {ok, UserId};
                        _Otherwise ->
                            lager:info("twitch access-token validation of '~s' failed: ~p", [AccessToken, Body]),
                            error
                    end;
                Error ->
                    lager:info("twitch access-token validation of '~s' failed: ~p", [AccessToken, Error]),
                    error
            end;
        _Error ->
            error
    end.


-spec refresh_token(user()) -> {ok, oauth_access()} | error.
refresh_token(User) ->
    case gen_server:call(?MODULE, {refresh_token_request, User}) of
        {ok, Target} ->
            case ogonek_util:json_post(Target) of
                {ok, 200, _Hs, Body} ->
                    OAuth = User#user.oauth,
                    extract_refresh_token(Body, OAuth);
                Error ->
                    lager:warning("refresh-token failed: ~p", [Error]),
                    error
            end;
        Error ->
            Error
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    gen_server:cast(self(), prepare),

    {ok, #state{enabled=false}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_info, _From, #state{enabled=false}=State) ->
    {reply, {error, inactive}, State};

handle_call(get_info, _From, State) ->
    AuthInfo = auth_info(State),
    {reply, {ok, AuthInfo}, State};

handle_call(get_client_id, _From, #state{enabled=false}=State) ->
    {reply, {error, inactive}, State};

handle_call(get_client_id, _From, State) ->
    {reply, {ok, State#state.client_id}, State};

handle_call({auth_token_request, _Code}, _From, #state{enabled=false}=State) ->
    {reply, {error, inactive}, State};

handle_call({auth_token_request, Code}, _From, State) ->
    {reply, {ok, build_auth_request(Code, State)}, State};

handle_call({refresh_token_request, _User}, _From, #state{enabled=false}=State) ->
    {reply, {error, inactive}, State};

handle_call({refresh_token_request, User}, _From, State) ->
    {reply, {ok, build_refresh_token_request(User, State)}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(prepare, State) ->
    ClientId0 = os:getenv("TWITCH_CLIENT_ID"),
    ClientSecret0 = os:getenv("TWITCH_CLIENT_SECRET"),

    case {ClientId0, ClientSecret0} of
        {false, _} ->
            lager:warning("twitch: missing TWITCH_CLIENT_ID"),
            {noreply, #state{enabled=false}};
        {_, false} ->
            lager:warning("twitch: missing TWITCH_CLIENT_SECRET"),
            {noreply, #state{enabled=false}};
        _Correct ->
            ClientId = list_to_binary(ClientId0),
            ClientSecret = list_to_binary(ClientSecret0),
            RedirectUri = list_to_binary(os:getenv("TWITCH_REDIRECT_URI", "http://localhost:8000/auth")),

            {noreply, State#state{enabled=true,
                                  client_id=ClientId,
                                  client_secret=ClientSecret,
                                  redirect_uri=RedirectUri
                                 }}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

auth_info(#state{client_id=ClientId, redirect_uri=RedirectUri}) ->
    Url = <<"https://id.twitch.tv/oauth2/authorize",
            "?client_id=", ClientId/binary,
            "&redirect_uri=", RedirectUri/binary,
            "&response_type=code",
            "&scope=openid+user:read:email">>,

    {[{?MSG_TYPE, <<"authinfo">>},
      {<<"provider">>, <<"twitch">>},
      {<<"loginUrl">>, Url}
     ]}.


build_auth_request(Code, State) ->
    ClientId = State#state.client_id,
    ClientSecret = State#state.client_secret,
    RedirectUri = State#state.redirect_uri,

    Target = <<"https://id.twitch.tv/oauth2/token",
               "?client_id=", ClientId/binary,
               "&client_secret=", ClientSecret/binary,
               "&code=", Code/binary,
               "&grant_type=authorization_code",
               "&redirect_uri=", RedirectUri/binary>>,
    lager:debug("twitch auth request: ~p", [Target]),

    Target.


build_refresh_token_request(User, State) ->
    ClientId = State#state.client_id,
    ClientSecret = State#state.client_secret,
    OAuth = User#user.oauth,
    RefreshToken = OAuth#oauth_access.refresh_token,

    Target = <<"https://id.twitch.tv/oauth2/token",
               "?client_id=", ClientId/binary,
               "&client_secret=", ClientSecret/binary,
               "&refresh_token=", RefreshToken/binary,
               "&grant_type=refresh_token">>,
    lager:debug("twitch refresh-token request: ~p", [Target]),

    Target.


extract_oauth_access(Json) ->
    % TODO: validate types and correctness
    Keys = [<<"access_token">>,
            <<"id_token">>,
            <<"refresh_token">>,
            <<"scope">>,
            <<"token_type">>],

    case ogonek_util:keys(Keys, Json) of
        [AToken, IdToken, Refresh, Scope, TType] ->
            {ok, #oauth_access{access_token=AToken,
                               id_token=IdToken,
                               refresh_token=Refresh,
                               scope=Scope,
                               token_type=TType
                              }};
        _Otherwise ->
            lager:warning("unexpected or incomplete oauth token received: ~p", [Json]),
            error
    end.


extract_refresh_token(Json, OAuth) ->
    % TODO: validate types and correctness
    Keys = [<<"access_token">>,
            <<"refresh_token">>,
            <<"scope">>],

    case ogonek_util:keys(Keys, Json) of
        [AToken, Refresh, Scope] ->
            Updated = OAuth#oauth_access{access_token=AToken,
                                         refresh_token=Refresh,
                                         scope=Scope},
            {ok, Updated};
        _Otherwise ->
            lager:warning("unexpected or incomplete oauth refresh-token received: ~p", [Json]),
            error
    end.


extract_user({Json}) ->
    Data = proplists:get_value(<<"data">>, Json, []),
    case Data of
        [Fst] ->
            Keys = [<<"id">>, <<"display_name">>, <<"email">>, <<"profile_image_url">>],
            case ogonek_util:keys(Keys, Fst) of
                [Id, Name, Email, Image] ->
                    {ok, #twitch_user{id=Id,
                                      display_name=Name,
                                      email=Email,
                                      profile_image_url=Image}};
                _Otherwise ->
                    error
            end;
        _ -> error
    end.


%%
%% Tests
%%

-ifdef(TEST).

extract_oauth_access_test_() ->
    Input = {[{<<"data">>,
               [{[{<<"id">>,<<"1">>},
                  {<<"login">>,<<"kongo2002">>},
                  {<<"display_name">>,<<"kongo2002">>},
                  {<<"type">>,<<>>},
                  {<<"broadcaster_type">>,<<>>},
                  {<<"description">>,<<>>},
                  {<<"profile_image_url">>,<<"https://some.where.com">>},
                  {<<"offline_image_url">>,<<>>},
                  {<<"view_count">>,6},
                  {<<"email">>,<<"kongo2002@gmail.com">>}]}]}]},

    [?_assertEqual({ok, #twitch_user{id= <<"1">>,
                                     display_name= <<"kongo2002">>,
                                     email= <<"kongo2002@gmail.com">>,
                                     profile_image_url= <<"https://some.where.com">>}},
                   extract_user(Input))].

-endif.
