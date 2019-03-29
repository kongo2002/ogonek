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

-module(ogonek_user_session).

-include("include/ogonek.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(USER_TIMEOUT_MILLIS, 120000).


-type socket_info() :: {binary(), pid()}.

-record(state, {
          id :: binary(),
          sockets :: [socket_info()],
          terminate :: reference() | undefined
         }).

-type state() :: #state{}.

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
start_link(UserId) ->
    gen_server:start_link(?MODULE, UserId, []).


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
init(UserId) ->
    {ok, #state{id=UserId, sockets=[]}}.

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
handle_cast({register_socket, Socket, SessionId}, State) ->
    Sockets = State#state.sockets,
    Sockets0 = lists:usort([{SessionId, Socket} | Sockets]),

    lager:debug("user-session ~s: ~p connections registered",
                [State#state.id, length(Sockets0)]),

    {noreply, check_activity(State#state{sockets=Sockets0})};

handle_cast({unregister_session, SessionId, Reason}, State) ->
    UserId = State#state.id,

    Logout = {logout, Reason},
    Sockets0 =
    lists:foldl(fun({Sess, Pid}, Ss) when Sess == SessionId ->
                        lager:debug("user-session ~s: unregistering socket ~p [~s] due to ~p",
                                    [UserId, Pid, SessionId, Reason]),
                        Pid ! Logout,
                        Ss;
                   (SessSock, Ss) ->
                        [SessSock | Ss]
                end, [], State#state.sockets),

    State0 = State#state{sockets=Sockets0},

    {noreply, check_remaining_sockets(State0)};

handle_cast({unregister_socket, Socket, SessionId}, State) ->
    Sockets = State#state.sockets,
    Sockets0 = lists:delete({SessionId, Socket}, Sockets),
    State0 = State#state{sockets=Sockets0},

    {noreply, check_remaining_sockets(State0)};

handle_cast({logout, Reason}, State) ->
    lager:info("user-session ~s: sending close to sockets due to ~p", [State#state.id, Reason]),
    publish_to_sockets({logout, Reason}, State),
    {noreply, State#state{sockets=[]}};

handle_cast({json_to_sockets, Json}, State) ->
    publish_to_sockets({json, Json}, State),
    {noreply, State};

handle_cast({json_to_sockets, Module, Objs}, State) when is_list(Objs) ->
    lists:foreach(fun(Obj) ->
                          Json = Module:to_json(Obj, false),
                          publish_to_sockets({json, Json}, State)
                  end, Objs),
    {noreply, State};

handle_cast({json_to_sockets, Module, Obj}, State) ->
    Json = Module:to_json(Obj, false),
    publish_to_sockets({json, Json}, State),
    {noreply, State};

handle_cast({publish_to_sockets, Msg}, State) ->
    publish_to_sockets(Msg, State),
    {noreply, State};

handle_cast({terminate, Reason}, State) ->
    lager:debug("user-session ~s - terminating due to ~p", [State#state.id, Reason]),
    {stop, normal, State};

handle_cast(Msg, State) ->
    lager:warning("user-session ~s - unhandled cast message: ~p", [State#state.id, Msg]),
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
handle_info(kill_timeout, State) ->
    case State#state.terminate of
        undefined ->
            % kill timeout is outdated
            % user is active again
            {noreply, State};
        _TimerRef ->
            lager:debug("user-session ~s - terminating due to inactivity", [State#state.id]),
            ogonek_session_manager:kill_timeout(State#state.id),
            {stop, normal, State}
    end;

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

-spec check_remaining_sockets(state()) -> state().
check_remaining_sockets(State) ->
    UserId = State#state.id,
    Sockets = State#state.sockets,

    case Sockets of
        [] ->
            lager:debug("user-session ~s: no remaining sockets", [UserId]),

            TimerRef = erlang:send_after(?USER_TIMEOUT_MILLIS, self(), kill_timeout),

            State#state{terminate=TimerRef};
        _NonEmpty ->
            lager:debug("user-session ~s: ~p connections registered",
                        [UserId, length(Sockets)]),
            State
    end.


-spec check_activity(state()) -> state().
check_activity(State) ->
    case State#state.terminate of
        undefined ->
            State;
        TimerRef ->
            erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
            State#state{terminate=undefined}
    end.


-spec publish_to_sockets(term(), state()) -> ok.
publish_to_sockets(Msg, #state{sockets=Sockets}) ->
    lists:foreach(fun({_SessionId, Socket}) -> Socket ! Msg end, Sockets).
