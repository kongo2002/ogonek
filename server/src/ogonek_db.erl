-module(ogonek_db).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(OGONEK_DB_NAME, <<"ogonek">>).

-record(state, {host, headers, options, status}).


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
    % TODO: configuration
    Host = <<"http://localhost:5984">>,

    Json = <<"application/json">>,
    DefaultHeaders = [{<<"Accept">>, Json},
                      {<<"Content-Type">>, Json}
                     ],

    DefaultOptions = [{pool, default}, with_body] ++ get_auth(),

    gen_server:cast(self(), prepare),

    {ok, #state{host=Host,
                headers=DefaultHeaders,
                options=DefaultOptions,
                status=init}}.

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
    lager:info("initializing database at ~s [~p]", [State#state.host, self()]),

    true = check_status(State),
    lager:debug("database connection is up and running"),

    ok = db_create_if_not_exists(?OGONEK_DB_NAME, State),

    {noreply, State#state{status=ready}};

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

get_auth() ->
    case {os:getenv("COUCH_USER"), os:getenv("COUCH_PW")} of
        {false, _} -> [];
        {_, false} -> [];
        {User, Pw} -> [{basic_auth, {list_to_binary(User), list_to_binary(Pw)}}]
    end.


get_(Path, #state{host=Host, headers=Headers, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    Result = hackney:get(Target, Headers, [], Options),

    lager:debug("GET: ~p", [Result]),
    Result.


head_(Path, #state{host=Host, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    Result = hackney:head(Target, [], [], Options),

    lager:debug("HEAD: ~p", [Result]),
    Result.


put_(Path, State) ->
    put_(Path, [], State).

put_(Path, Payload, #state{host=Host, headers=Headers, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    Result = hackney:put(Target, Headers, Payload, Options),

    lager:debug("PUT: ~p", [Result]),
    Result.


check_status(State) ->
    {ok, Code, _Hs, _Body} = get_(<<"/_up">>, State),
    Code == 200.


db_exists(Db, State) ->
    {ok, Code, _Hs} = head_(<<"/", Db/binary>>, State),
    Code == 200.


db_create(Db, State) ->
    {ok, Code, _Hs, Body} = put_(<<"/", Db/binary>>, State),
    case Code of
        201 ->
            lager:info("successfully created database '~s'", [Db]),
            ok;
        Error ->
            lager:error("failed to create database '~s' [~p]: ~p", [Db, Error, Body]),
            {error, Error}
    end.


db_create_if_not_exists(Db, State) ->
    case db_exists(Db, State) of
        true -> ok;
        false -> db_create(Db, State)
    end.
