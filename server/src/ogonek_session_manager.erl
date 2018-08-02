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
         register/3,
         close_socket/1,
         close_socket/2,
         logout/1,
         logout/2]).

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


-spec logout(binary()) -> ok.
logout(UserId) ->
    logout(self(), UserId).


-spec logout(pid(), binary()) -> ok.
logout(Socket, UserId) ->
    gen_server:cast(?MODULE, {logout, Socket, UserId}).


-spec close_socket(binary()) -> ok.
close_socket(UserId) ->
    close_socket(self(), UserId).


-spec close_socket(pid(), binary() | undefined) -> ok.
close_socket(_Socket, undefined) -> ok;
close_socket(Socket, UserId) ->
    gen_server:cast(?MODULE, {close_socket, Socket, UserId}).

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

    {ToClose, OldSession, Sessions0} = record_session(SessionId, Socket, Sessions),

    % close existing sockets of old/replaced session
    logout_sockets(ToClose, replaced_session),

    % moreover we are going to remove any user-id from the old session
    ogonek_db:remove_user_from_session(OldSession),

    % and associate the user-id with the new session instead
    ogonek_db:add_user_to_session(UserId, SessionId),

    {_, Sockets} = Sessions0,
    lager:info("registered ~p sockets at session '~s' for user '~s'",
               [length(Sockets), SessionId, UserId]),

    UserSessions0 = maps:put(UserId, Sessions0, UserSessions),

    {noreply, State#state{sessions=UserSessions0}};

handle_cast({logout, Socket, UserId}, State) ->
    lager:info("logout for user '~s' requested from ~p", [UserId, Socket]),

    UserSessions = State#state.sessions,

    case maps:get(UserId, UserSessions, undefined) of
        undefined ->
            lager:warning("session-manager: there are no sessions associated with user '~s'", [UserId]),
            ok;
        {Session, Sockets} ->
            % close connected sockets
            logout_sockets(Sockets, user_logout),

            % and remove user association from session
            ogonek_db:remove_user_from_session(Session)
    end,

    UserSessions0 = maps:remove(UserId, UserSessions),

    {noreply, State#state{sessions=UserSessions0}};

handle_cast({close_socket, Socket, UserId}, State) ->
    UserSessions = State#state.sessions,

    case maps:get(UserId, UserSessions) of
        undefined ->
            {noreply, State};
        {Session, Sockets} ->
            lager:debug("remove socket ~p from user '~s'", [Socket, UserId]),

            Sockets0 = lists:delete(Socket, Sockets),
            UserSessions0 = case Sockets0 of
                                [] -> maps:remove(UserId, UserSessions);
                                _ -> maps:put(UserId, {Session, Sockets0})
                            end,
            {noreply, State#state{sessions=UserSessions0}}
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

% no recorded session at all
record_session(SessionId, Socket, undefined) ->
    {[], undefined, {SessionId, [Socket]}};

% there are senders registered to the same session-id already
record_session(SessionId, Socket, {SessionId, Senders}) ->
    Sockets = lists:usort([Socket | Senders]),
    {[], undefined, {SessionId, Sockets}};

% a different session is registered already
% -> drop the existing senders and replace with the new one
record_session(SessionId, Socket, {OldSession, Senders}) ->
    {Senders, OldSession, {SessionId, [Socket]}}.


logout_sockets(Sockets, Reason) ->
    lists:foreach(fun(Socket) ->
                          lager:info("sending close to socket [~p] due to ~p", [Socket, Reason]),
                          Socket ! {logout, Reason}
                  end,
                  Sockets).
