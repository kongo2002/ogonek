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

-module(ogonek_db).

-include("ogonek.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Session API
-export([new_session/2,
         refresh_session/1,
         get_session/1,
         add_user_to_session/2,
         remove_user_from_session/1]).

%% User API
-export([create_user/2,
         get_user/1,
         get_user/2]).

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


new_session(RemoteIP, Headers) ->
    Headers0 = prepare_headers(Headers),
    Timestamp = iso8601:format(calendar:universal_time()),

    Session = #session{ip=RemoteIP,
                       created=Timestamp,
                       updated=Timestamp,
                       headers=Headers0
                      },
    Doc = ogonek_session:to_json(Session),

    gen_server:call(?MODULE, {new_session, Doc}).


get_session(Session) ->
    gen_server:call(?MODULE, {get_session, Session}).


refresh_session(Session) ->
    gen_server:cast(?MODULE, {refresh_session, Session}).


remove_user_from_session(undefined) -> ok;
remove_user_from_session(SessionId) ->
    gen_server:cast(?MODULE, {remove_user_from_session, SessionId}).


add_user_to_session(UserId, SessionId) ->
    gen_server:cast(?MODULE, {add_user_to_session, UserId, SessionId}).


create_user(User, Provider) ->
    gen_server:call(?MODULE, {create_user, User, Provider}).


get_user(UserId) ->
    gen_server:call(?MODULE, {get_user, UserId}).


get_user(ProviderId, Provider) ->
    gen_server:call(?MODULE, {get_user, ProviderId, Provider}).

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
handle_call({new_session, Doc}, _From, State) ->
    lager:debug("creating new session: ~p", [Doc]),

    Response = insert(Doc, State),
    {reply, Response, State};

handle_call({get_session, SessionId}, _From, State) ->
    Response = case get_by_id(SessionId, State) of
                   {ok, Code, _Hs, Session} when Code == 200 ->
                       ogonek_session:from_json(Session);
                   _Error ->
                       {error, not_found}
               end,

    {reply, Response, State};

handle_call({create_user, #twitch_user{}=User, Provider}, _From, State) ->
    Doc = ogonek_util:doc(<<"user">>,
                          [{<<"provider">>, Provider},
                           {<<"pid">>, User#twitch_user.id},
                           {<<"email">>, User#twitch_user.email},
                           {<<"name">>, User#twitch_user.display_name},
                           {<<"img">>, User#twitch_user.profile_image_url}
                          ]),
    Response = case insert(Doc, State) of
                   {ok, Id, _Rev} ->
                       ogonek_user:from_json(with_id(Doc, Id));
                   Error -> Error
               end,
    {reply, Response, State};

handle_call({get_user, UserId}, _From, State) ->
    Response = case get_by_id(UserId, State) of
                   {ok, Code, _Hs, User} when Code == 200 ->
                       ogonek_user:from_json(User);
                   _Error ->
                       {error, not_found}
               end,

    {reply, Response, State};

handle_call({get_user, ProviderId, Provider}, _From, State) ->
    % query the 'user' design doc views
    Response = case singleton_from_view(<<"user">>, Provider, ProviderId, State) of
                   {ok, _Key, User} ->
                       ogonek_user:from_json(User);
                   Error ->
                       Error
               end,

    {reply, Response, State};

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

handle_cast({refresh_session, Session}, State) ->
    Now = iso8601:format(calendar:universal_time()),
    Update = fun(_Code, {S}) ->
                     Ts = <<"updated">>,
                     Updated = lists:keyreplace(Ts, 1, S, {Ts, Now}),
                     {Updated}
             end,

    case update(Session, Update, State) of
        {ok, Code, _Hs, _Body} when Code == 200 orelse Code == 201 -> ok;
        {ok, Code, _Hs, Body} ->
            lager:error("refresh_session failed [~p]: ~p", [Code, Body]),
            error;
        Error ->
            lager:error("refresh_session failed: ~p", [Error]),
            error
    end,

    {noreply, State};

handle_cast({add_user_to_session, UserId, SessionId}, State) ->
    Now = iso8601:format(calendar:universal_time()),
    NewKeys = [{<<"updated">>, Now}, {<<"user_id">>, UserId}],
    Update = fun(_Code, {S}) ->
                     Updated = ogonek_util:replace_with(S, NewKeys),
                     {Updated}
             end,

    case update(SessionId, Update, State) of
        {ok, Code, _Hs, _Body} when Code == 200 orelse Code == 201 -> ok;
        {ok, Code, _Hs, Body} ->
            lager:error("add_user_to_session failed [~p]: ~p", [Code, Body]),
            error;
        Error ->
            lager:error("add_user_to_session failed: ~p", [Error]),
            error
    end,

    {noreply, State};

handle_cast({remove_user_from_session, SessionId}, State) ->
    Now = iso8601:format(calendar:universal_time()),
    Update = fun(_Code, {S}) ->
                     Ts = <<"updated">>,
                     Updated0 = lists:keyreplace(Ts, 1, S, {Ts, Now}),
                     Updated1 = lists:keydelete(<<"user_id">>, 1, Updated0),
                     {Updated1}
             end,

    case update(SessionId, Update, State) of
        {ok, Code, _Hs, _Body} when Code == 200 orelse Code == 201 -> ok;
        {ok, Code, _Hs, Body} ->
            lager:error("remove_user_from_session failed [~p]: ~p", [Code, Body]),
            error;
        Error ->
            lager:error("remove_user_from_session failed: ~p", [Error]),
            error
    end,

    {noreply, State};

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
    ogonek_util:json_get(Target, Headers, Options).


get_by_id(Id, State) ->
    get_by_id(?OGONEK_DB_NAME, Id, State).

get_by_id(Db, Id, State) ->
    Path = <<"/", Db/binary, "/", Id/binary>>,
    get_(Path, State).


head_(Path, #state{host=Host, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    Result = hackney:head(Target, [], [], Options),

    lager:debug("HEAD: ~p", [Result]),
    Result.


put_(Path, State) ->
    put_(Path, [], State).

put_(Path, Payload, #state{host=Host, headers=Headers, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    Json = jiffy:encode(Payload),
    Result = hackney:put(Target, Headers, Json, Options),

    lager:debug("PUT: ~p", [Result]),
    Result.


post_(Path, Payload, #state{host=Host, headers=Headers, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    ogonek_util:json_post(Target, Headers, Payload, Options).


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


insert(Doc, State) ->
    insert(?OGONEK_DB_NAME, Doc, State).

insert(Db, Doc, State) ->
    case post_(<<"/", Db/binary>>, Doc, State) of
        % created or accepted
        {ok, Code, _Hs, Body} when Code == 201 orelse Code == 202 ->
            parse_id_rev(Body);
        {ok, Code, _Hs, Body} ->
            lager:error("insert [POST] of document ~p failed [~p]: ~p", [Doc, Code, Body]),
            {error, {status, Code}};
        Unexpected ->
            lager:error("insert [POST] of document ~p failed unexpectedly: ~p", [Doc, Unexpected]),
            {error, unexpected}
    end.


update(Id, Func, State) ->
    update(?OGONEK_DB_NAME, Id, Func, State).

update(Db, Id, Func, State) ->
    Path = <<"/", Db/binary, "/", Id/binary>>,

    case get_(Path, State) of
        {ok, Code, _Hs, Body} ->
            Updated = Func(Code, Body),
            put_(Path, Updated, State);
        Otherwise ->
            Otherwise
    end.


prepare_headers(Headers) ->
    % TODO: filter out 'interesting' headers only?
    lists:foldl(fun({H, V}, Hs) ->
                    [{ogonek_util:lowercase(H), V} | Hs]
                end, [], Headers).


parse_id_rev({Json}) ->
    Id = proplists:get_value(<<"id">>, Json),
    Rev = proplists:get_value(<<"rev">>, Json),
    case {Id, Rev} of
        {undefined, _} -> {error, missing_id};
        {_, undefined} -> {error, missing_rev};
        _ -> {ok, Id, Rev}
    end.


singleton_from_view(Design, View, Key, State) ->
    singleton_from_view(?OGONEK_DB_NAME, Design, View, Key, State).

singleton_from_view(Db, Design, View, Key, State) ->
    Target = <<"/", Db/binary, "/_design/", Design/binary,
               "/_view/", View/binary,
               "?key=\"", Key/binary, "\"">>,

    case get_(Target, State) of
        {ok, Code, _Hs, Body} when Code == 200 ->
            case ogonek_util:keys([<<"rows">>], Body) of
                [[Singleton]] ->
                    case ogonek_util:keys([<<"key">>, <<"value">>], Singleton) of
                        [Key, Value] ->
                            {ok, Key, Value};
                        Unexpected ->
                            lager:error("received unexpected result from view: ~p", [Unexpected]),
                            error
                    end;
                [] -> {error, not_found};
                [[]] -> {error, not_found};
                [_Multiple] ->
                    lager:error("received multiple results from view where only one was expected: '~s'", [Target]),
                    {error, multiple}
            end;
        _Error ->
            {error, not_found}
    end.


with_id({Doc}, Id) ->
    with_id(Doc, Id);

with_id(Doc, Id) ->
    {[{<<"_id">>, Id} | Doc]}.
