-module(ogonek_twitch).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([get_info/0,
         get_auth_token/1]).

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
            Opts = [{pool, default}, with_body],
            Result = hackney:post(Target, [], [], Opts),
            lager:debug("twitch auth response: ~p", [Result]),
            Result;
        Error -> Error
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

handle_call({auth_token_request, _Code}, _From, #state{enabled=false}=State) ->
    {reply, {error, inactive}, State};

handle_call({auth_token_request, Code}, _From, State) ->
    {reply, {ok, build_auth_request(Code, State)}, State};

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
            "&scope=openid user:read:email">>,

    {[{<<"_t">>, <<"authinfo">>},
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
