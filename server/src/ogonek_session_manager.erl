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

-export([register_socket/2,
         register_socket/3,
         publish_to_user/2,
         publish_to_user/3,
         publish_to_sockets/2,
         close_socket/1,
         close_socket/2,
         kill_timeout/1,
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


-spec register_socket(binary(), binary()) -> ok.
register_socket(UserId, SessionId) ->
    register_socket(self(), UserId, SessionId).


-spec register_socket(pid(), binary(), binary()) -> ok.
register_socket(Socket, UserId, SessionId) ->
    gen_server:cast(?MODULE, {register_socket, Socket, UserId, SessionId}).


-spec publish_to_user(binary(), term()) -> ok.
publish_to_user(UserId, Msg) ->
    gen_server:cast(?MODULE, {publish_to_user, UserId, undefined, Msg}).


-spec publish_to_user(binary(), binary(), term()) -> ok.
publish_to_user(UserId, SessionId, Msg) ->
    gen_server:cast(?MODULE, {publish_to_user, UserId, SessionId, Msg}).


-spec publish_to_sockets(binary(), term()) -> ok.
publish_to_sockets(UserId, Msg) ->
    gen_server:cast(?MODULE, {publish_to_sockets, UserId, Msg}).


-spec kill_timeout(UserId :: binary()) -> ok.
kill_timeout(UserId) ->
    gen_server:cast(?MODULE, {kill_timeout, UserId}).


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
handle_cast({register_socket, Socket, UserId, SessionId}, State) ->
    UserSessions = State#state.sessions,
    Sessions = maps:get(UserId, UserSessions, undefined),

    {OldSession, Sessions0} = record_session(UserId, SessionId, Socket, Sessions),

    % moreover we are going to remove any user-id from the old session
    ogonek_db:remove_user_from_session(OldSession),

    % and associate the user-id with the new session instead
    ogonek_db:add_user_to_session(UserId, SessionId),

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
            Dispatcher = Session#user_session.dispatcher,
            gen_server:cast(Dispatcher, {publish_to_sockets, Msg})
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
            logout_sockets(Session, user_logout),

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
            SessionId = Session#user_session.session_id,
            lager:debug("remove socket ~p from user '~s'", [Socket, UserId]),

            % unregister socket at user's session dispatcher as well
            gen_server:cast(Session#user_session.dispatcher, {unregister_socket, Socket, SessionId}),

            {noreply, State}
    end;

handle_cast({kill_timeout, UserId}, State) ->
    UserSessions = State#state.sessions,

    case maps:get(UserId, UserSessions, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            Msg = {terminate, kill_timeout},
            gen_server:cast(Session#user_session.lifecycle, Msg),

            UserSessions0 = maps:remove(UserId, UserSessions),
            {noreply, State#state{sessions=UserSessions0}}
    end;

handle_cast(Msg, State) ->
    lager:warning("session-manager - unhandled cast message: ~p", [Msg]),
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
handle_info(Info, State) ->
    lager:warning("session-manager - unhandled message: ~p", [Info]),
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

-spec record_session(binary(), binary(), pid(), user_session() | undefined) ->
    {binary() | undefined, user_session()}.

% no recorded session at all
record_session(UserId, SessionId, Socket, undefined) ->
    {ok, SessionDispatcher} = ogonek_user_session:start_link(UserId),
    {ok, NewLifecycle} = ogonek_user_lifecycle_sup:start_user_lifecycle(UserId, SessionDispatcher),

    NewSession = #user_session{user_id=UserId,
                               session_id=SessionId,
                               lifecycle=NewLifecycle,
                               dispatcher=SessionDispatcher},

    % register new socket with user's session dispatcher as well
    dispatch_register_socket(NewSession, Socket, SessionId),

    {undefined, NewSession};

% there are senders registered to the same session-id already
record_session(_UserId, SessionId, Socket, #user_session{session_id=SessionId}=Session) ->
    % register new socket with user's session dispatcher
    dispatch_register_socket(Session, Socket, SessionId),

    {undefined, Session};

% a different session is registered already
% -> drop the existing senders and replace with the new one
record_session(_UserId, SessionId, Socket, Session) ->
    OldSession = Session#user_session.session_id,
    NewSession = Session#user_session{session_id=SessionId},

    % register new socket at user's session dispatcher
    dispatch_register_socket(Session, Socket, SessionId),

    % unregister old session at user's session dispatcher
    unregister_session(NewSession, OldSession, replaced_session),

    {OldSession, NewSession}.


-spec dispatch_register_socket(user_session(), pid(), binary()) -> ok.
dispatch_register_socket(Session, Socket, SessionId) ->
    to_dispatcher(Session, {register_socket, Socket, SessionId}).


-spec unregister_session(user_session(), binary(), atom()) -> ok.
unregister_session(Session, SessionId, Reason) ->
    to_dispatcher(Session, {unregister_session, SessionId, Reason}).


-spec logout_sockets(user_session(), atom()) -> ok.
logout_sockets(Session, Reason) ->
    to_dispatcher(Session, {logout, Reason}).


-spec terminate_lifecycle(user_session(), atom()) -> ok.
terminate_lifecycle(#user_session{lifecycle=Pid}, Reason) ->
    gen_server:cast(Pid, {terminate, Reason}).


-spec to_dispatcher(user_session(), term()) -> ok.
to_dispatcher(Session, Msg) ->
    Dispatcher = Session#user_session.dispatcher,
    gen_server:cast(Dispatcher, Msg).
