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

-module(ogonek_session_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([register/2,
         register/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {sessions :: map()}).

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


-spec register(binary(), binary()) -> ok.
register(UserId, SessionId) ->
    register(self(), UserId, SessionId).


-spec register(pid(), binary(), binary()) -> ok.
register(Socket, UserId, SessionId) ->
    gen_server:cast(?MODULE, {register, Socket, UserId, SessionId}).


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
    lager:info("initializing session manager [~p]", [self()]),
    {ok, #state{sessions=maps:new()}}.

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
handle_cast({register, Socket, UserId, SessionId}, State) ->
    UserSessions = State#state.sessions,
    Sessions = maps:get(UserId, UserSessions, undefined),

    {ToClose, Sessions0} = record_session(SessionId, Socket, Sessions),

    % close existing sockets of old/replaced session
    lists:foreach(fun(Close) ->
                          lager:info("sending close to socket [~p] of replaced session", [Close]),
                          Close ! {logout, replaced_session}
                  end,
                  ToClose),

    {_, Sockets} = Sessions0,
    lager:info("registered ~p sockets at session '~s' for user '~s'",
               [length(Sockets), SessionId, UserId]),

    UserSessions0 = maps:put(UserId, Sessions0, UserSessions),

    {noreply, State#state{sessions=UserSessions0}};

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

% no recorded session at all
record_session(SessionId, Socket, undefined) ->
    {[], {SessionId, [Socket]}};

% there are senders registered to the same session-id already
record_session(SessionId, Socket, {SessionId, Senders}) ->
    Sockets = lists:usort([Socket | Senders]),
    {[], {SessionId, Sockets}};

% a different session is registered already
% -> drop the existing senders and replace with the new one
record_session(SessionId, Socket, {_OtherSession, Senders}) ->
    {Senders, {SessionId, [Socket]}}.
