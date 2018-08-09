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
         publish_to_user/3,
         publish_to_sockets/2,
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

-record(user_session, {
          user_id :: binary(),
          session_id :: binary(),
          sockets :: [pid()],
          lifecycle :: pid(),
          dispatcher :: pid()
         }).

-type user_session() :: #user_session{}.

-record(state, {sessions :: #{binary() => user_session()}}).

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


-spec publish_to_user(binary(), binary(), term()) -> ok.
publish_to_user(UserId, SessionId, Msg) ->
    gen_server:cast(?MODULE, {publish_to_user, UserId, SessionId, Msg}).


-spec publish_to_sockets(binary(), term()) -> ok.
publish_to_sockets(UserId, Msg) ->
    gen_server:cast(?MODULE, {publish_to_sockets, UserId, Msg}).


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

    {ToClose, OldSession, Sessions0} = record_session(UserId, SessionId, Socket, Sessions),

    % close existing sockets of old/replaced session
    logout_sockets(ToClose, replaced_session),

    % moreover we are going to remove any user-id from the old session
    ogonek_db:remove_user_from_session(OldSession),

    % and associate the user-id with the new session instead
    ogonek_db:add_user_to_session(UserId, SessionId),

    lager:info("registered ~p sockets at session '~s' for user '~s'",
               [length(Sessions0#user_session.sockets), SessionId, UserId]),

    UserSessions0 = maps:put(UserId, Sessions0, UserSessions),

    {noreply, State#state{sessions=UserSessions0}};

handle_cast({publish_to_user, UserId, _SessionId, Msg}, State) ->
    case maps:get(UserId, State#state.sessions, undefined) of
        undefined -> ok;
        Session ->
            Lifecycle = Session#user_session.lifecycle,
            Lifecycle ! Msg
    end,
    {noreply, State};

handle_cast({publish_to_sockets, UserId, Msg}, State) ->
    case maps:get(UserId, State#state.sessions, undefined) of
        undefined -> ok;
        Session ->
            lists:foreach(fun(S) -> S ! Msg end, Session#user_session.sockets)
    end,
    {noreply, State};

handle_cast({logout, Socket, UserId}, State) ->
    lager:info("logout for user '~s' requested from ~p", [UserId, Socket]),

    UserSessions = State#state.sessions,

    case maps:get(UserId, UserSessions, undefined) of
        undefined ->
            lager:warning("session-manager: there are no sessions associated with user '~s'", [UserId]),
            ok;
        Session ->
            % close connected sockets
            logout_sockets(Session#user_session.sockets, user_logout),

            % unregister remaining sockets from user's session dispatcher
            unregister_dispatcher_sockets(Session),

            % and remove user association from session
            ogonek_db:remove_user_from_session(Session#user_session.session_id),

            % and terminate the associated user lifecycle as well
            terminate_lifecycle(Session, logout)
    end,

    UserSessions0 = maps:remove(UserId, UserSessions),

    {noreply, State#state{sessions=UserSessions0}};

handle_cast({close_socket, Socket, UserId}, State) ->
    UserSessions = State#state.sessions,

    case maps:get(UserId, UserSessions, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            lager:debug("remove socket ~p from user '~s'", [Socket, UserId]),

            % unregister socket at user's session dispatcher as well
            gen_server:cast(Session#user_session.dispatcher, {unregister_socket, Socket}),

            Sockets0 = lists:delete(Socket, Session#user_session.sockets),
            UserSessions0 = case Sockets0 of
                                [] ->
                                    % there are no more sockets associated with this user's session
                                    % let's remove the stored session and terminate its lifecycle
                                    terminate_lifecycle(Session, connection_close),
                                    maps:remove(UserId, UserSessions);
                                _ ->
                                    % update user session as there are still open sockets registered
                                    Session0 = Session#user_session{sockets=Sockets0},
                                    maps:put(UserId, Session0, UserSessions)
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
record_session(UserId, SessionId, Socket, undefined) ->
    {ok, SessionDispatcher} = ogonek_user_session:start_link(UserId),
    {ok, NewLifecycle} = ogonek_user_lifecycle_sup:start_user_lifecycle(UserId, SessionDispatcher),

    NewSession = #user_session{user_id=UserId,
                               session_id=SessionId,
                               lifecycle=NewLifecycle,
                               dispatcher=SessionDispatcher,
                               sockets=[Socket]},

    % register new socket with user's session dispatcher as well
    gen_server:cast(SessionDispatcher, {register_socket, Socket}),

    {[], undefined, NewSession};

% there are senders registered to the same session-id already
record_session(_UserId, SessionId, Socket, #user_session{session_id=SessionId}=Session) ->
    Sockets = Session#user_session.sockets,
    Sockets0 = lists:usort([Socket | Sockets]),
    Session0 = Session#user_session{sockets=Sockets0},

    % register new socket with user's session dispatcher
    gen_server:cast(Session#user_session.dispatcher, {register_socket, Socket}),

    {[], undefined, Session0};

% a different session is registered already
% -> drop the existing senders and replace with the new one
record_session(_UserId, SessionId, Socket, Session) ->
    OldSockets = Session#user_session.sockets,
    OldSession = Session#user_session.session_id,
    NewSession = Session#user_session{session_id=SessionId,
                                      sockets=[Socket]},

    % register new socket at user's session dispatcher
    Dispatcher = Session#user_session.dispatcher,
    gen_server:cast(Dispatcher, {register_socket, Socket}),

    % unregister all old sockets from user's session dispatcher
    lists:foreach(fun(S) ->
                          gen_server:cast(Dispatcher, {unregister_socket, S})
                  end, OldSockets),

    {OldSockets, OldSession, NewSession}.


logout_sockets(Sockets, Reason) ->
    lists:foreach(fun(Socket) ->
                          lager:info("sending close to socket [~p] due to ~p", [Socket, Reason]),
                          Socket ! {logout, Reason}
                  end,
                  Sockets).


-spec unregister_dispatcher_sockets(user_session()) -> ok.
unregister_dispatcher_sockets(Session) ->
    Dispatcher = Session#user_session.dispatcher,
    lists:foreach(fun(S) ->
                          gen_server:cast(Dispatcher, {unregister_socket, S})
                  end, Session#user_session.sockets).


-spec terminate_lifecycle(user_session(), atom()) -> ok.
terminate_lifecycle(#user_session{lifecycle=Pid}, Reason) ->
    gen_server:cast(Pid, {terminate, Reason}).
