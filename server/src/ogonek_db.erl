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
-export([create_user_from_twitch/2,
         update_user/1,
         get_user/1,
         get_user/2]).

%% Planet API
-export([planet_exists/3,
         planet_create/1,
         planet_claim/2,
         planet_free/0,
         planets_of_user/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(OGONEK_DB_NAME, <<"ogonek">>).

-record(db_info, {host, headers, options}).
-type db_info() :: #db_info{}.

-record(state, {info, status}).


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


-spec new_session(binary(), [kvalue()]) ->
    {ok, binary(), binary()} |
    {error, missing_id} |
    {error, missing_rev}.
new_session(RemoteIP, Headers) ->
    Headers0 = prepare_headers(Headers),
    Timestamp = iso8601:format(calendar:universal_time()),

    Session = #session{ip=RemoteIP,
                       created=Timestamp,
                       updated=Timestamp,
                       headers=Headers0
                      },
    Doc = ogonek_session:to_json(Session),

    lager:debug("creating new session: ~p", [Doc]),

    insert(Doc, get_info()).


-spec get_session(binary()) -> {ok, session()} | {error, not_found} | {error, invalid}.
get_session(SessionId) ->
    case get_by_id(SessionId, get_info()) of
        {ok, Code, _Hs, Session} when Code == 200 ->
            ogonek_session:from_json(Session);
        _Error ->
            {error, not_found}
    end.


-spec refresh_session(binary()) -> ok.
refresh_session(Session) ->
    gen_server:cast(?MODULE, {refresh_session, Session}).


-spec remove_user_from_session(binary() | undefined) -> ok.
remove_user_from_session(undefined) -> ok;
remove_user_from_session(SessionId) ->
    gen_server:cast(?MODULE, {remove_user_from_session, SessionId}).


-spec add_user_to_session(binary(), binary()) -> ok.
add_user_to_session(UserId, SessionId) ->
    gen_server:cast(?MODULE, {add_user_to_session, UserId, SessionId}).


-spec create_user_from_twitch(twitch_user(), binary()) ->
    {ok, user()} |
    {error, invalid} |
    {error, missing_id} |
    {error, missing_rev}.
create_user_from_twitch(User, Provider) ->
    Values = [{<<"provider">>, Provider},
              {<<"pid">>, User#twitch_user.id},
              {<<"email">>, User#twitch_user.email},
              {<<"name">>, User#twitch_user.display_name},
              {<<"img">>, User#twitch_user.profile_image_url}
             ],
    Doc = ogonek_util:doc(<<"user">>, Values),

    case insert(Doc, get_info()) of
        {ok, Id, _Rev} ->
            ogonek_user:from_json(with_id(Doc, Id));
        Error -> Error
    end.


-spec get_user(binary()) ->
    {ok, user()} |
    {error, invalid} |
    {error, not_found}.
get_user(UserId) ->
    case get_by_id(UserId, get_info()) of
        {ok, Code, _Hs, User} when Code == 200 ->
            ogonek_user:from_json(User);
        _Error ->
            {error, not_found}
    end.


-spec get_user(binary(), binary()) ->
    {ok, user()} |
    {error, invalid} |
    {error, multiple} |
    {error, not_found} |
    error.
get_user(ProviderId, Provider) ->
    % query the 'user' design doc views
    case singleton_from_view(<<"user">>, Provider, ProviderId, get_info()) of
        {ok, _Key, User} ->
            ogonek_user:from_json(User);
        Error ->
            Error
    end.


-spec update_user(user()) -> ok.
update_user(User) ->
    gen_server:cast(?MODULE, {update_user, User}).


-spec planet_exists(integer(), integer(), integer()) -> boolean().
planet_exists(X, Y, Z) ->
    case singleton_from_view(<<"planet">>, <<"by_coordinate">>, [X, Y, Z], get_info()) of
        {ok, _Key, _Value} -> true;
        _Otherwise -> false
    end.


-spec planet_create(planet()) -> ok.
planet_create(Planet) ->
    gen_server:cast(?MODULE, {planet_create, Planet}).


-spec planet_claim(planet(), binary()) -> ok.
planet_claim(Planet, UserId) ->
    Updated = Planet#planet{owner=UserId},
    gen_server:cast(?MODULE, {planet_update, Updated}).


-spec planet_free() -> {ok, planet()} | {error, not_found} | {error, invalid}.
planet_free() ->
    planet_free0(25, get_info()).


planet_free0(Limit, Info) ->
    planet_free0(0, Limit, Info).

planet_free0(Skip, Limit, Info) ->
    OwnerKey = [<<"owner">>],
    PlanetFree = fun(Planet) ->
                         case ogonek_util:keys(OwnerKey, Planet) of
                             [] -> true;
                             [undefined] -> true;
                             [null] -> true;
                             _Otherwise -> false
                         end
                 end,

    Results = list_from_view(<<"planet">>, <<"by_coordinate">>, Skip, Limit, Info),

    case Results of
        [] -> {error, not_found};
        Res ->
            case lists:filter(PlanetFree, Res) of
                [Hd | _] ->
                    ogonek_planet:from_json(Hd);
                _Otherwise ->
                    planet_free0(Skip + Limit, Limit, Info)
            end
    end.


-spec planets_of_user(binary()) -> [planet()].
planets_of_user(UserId) ->
    Results = from_view(<<"planet">>, <<"by_owner">>, UserId, get_info()),
    lists:flatmap(fun(Planet) ->
                          case ogonek_planet:from_json(Planet) of
                              {ok, P} -> [P];
                              _Otherwise -> []
                          end
                  end, Results).


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

    Info = #db_info{host=Host,
                    headers=DefaultHeaders,
                    options=DefaultOptions},

    {ok, #state{info=Info,
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
handle_call(get_info, _From, State) ->
    Info = State#state.info,
    {reply, Info, State};

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
handle_cast(prepare, #state{info=Info}=State) ->
    lager:info("initializing database at ~s [~p]", [Info#db_info.host, self()]),

    true = check_status(Info),
    lager:debug("database connection is up and running"),

    ok = db_create_if_not_exists(?OGONEK_DB_NAME, Info),

    ok = design_create_if_not_exists(<<"session">>,
<<"{
  \"views\": {
    \"by_updated\": {
      \"map\": \"function(doc) { if (doc.updated && doc.t == 'session') { emit(doc.updated, doc) } }\"
    },
    \"by_ip\": {
      \"map\": \"function(doc) { if (doc.ip && doc.t == 'session') { emit(doc.ip, doc) } }\"
    }
  }
}">>, Info),

    ok = design_create_if_not_exists(<<"user">>,
<<"{
  \"views\": {
    \"twitter\": {
      \"map\": \"function(doc) { if (doc.pid && doc.t == 'user' && doc.provider == 'twitch') { emit(doc.pid, doc) } }\"
    },
    \"local\": {
      \"map\": \"function(doc) { if (doc.pid && doc.t == 'user' && doc.provider == 'local') { emit(doc.pid, doc) } }\"
    }
  }
}">>, Info),

    ok = design_create_if_not_exists(<<"planet">>,
<<"{
  \"views\": {
    \"by_coordinate\": {
      \"map\": \"function(doc) { if (doc.t == 'planet' && doc.pos) { emit(doc.pos, doc) } }\"
    },
    \"by_owner\": {
      \"map\": \"function(doc) { if (doc.t == 'planet' && doc.owner) { emit(doc.owner, doc) } }\"
    }
  }
}">>, Info),

    {noreply, State#state{status=ready}};

handle_cast({refresh_session, Session}, #state{info=Info}=State) ->
    Now = iso8601:format(calendar:universal_time()),
    Update = fun(_Code, {S}) ->
                     Ts = <<"updated">>,
                     Updated = lists:keyreplace(Ts, 1, S, {Ts, Now}),
                     {Updated}
             end,

    case update(Session, Update, Info) of
        {ok, Code, _Hs, _Body} when Code == 200 orelse Code == 201 -> ok;
        {ok, Code, _Hs, Body} ->
            lager:error("refresh_session failed [~p]: ~p", [Code, Body]),
            error;
        Error ->
            lager:error("refresh_session failed: ~p", [Error]),
            error
    end,

    {noreply, State};

handle_cast({add_user_to_session, UserId, SessionId}, #state{info=Info}=State) ->
    Now = iso8601:format(calendar:universal_time()),
    NewKeys = [{<<"updated">>, Now}, {<<"user_id">>, UserId}],
    Update = fun(_Code, {S}) ->
                     Updated = ogonek_util:replace_with(S, NewKeys),
                     {Updated}
             end,

    case update(SessionId, Update, Info) of
        {ok, Code, _Hs, _Body} when Code == 200 orelse Code == 201 -> ok;
        {ok, Code, _Hs, Body} ->
            lager:error("add_user_to_session failed [~p]: ~p", [Code, Body]),
            error;
        Error ->
            lager:error("add_user_to_session failed: ~p", [Error]),
            error
    end,

    {noreply, State};

handle_cast({remove_user_from_session, SessionId}, #state{info=Info}=State) ->
    Now = iso8601:format(calendar:universal_time()),
    Update = fun(_Code, {S}) ->
                     Ts = <<"updated">>,
                     Updated0 = lists:keyreplace(Ts, 1, S, {Ts, Now}),
                     Updated1 = lists:keydelete(<<"user_id">>, 1, Updated0),
                     {Updated1}
             end,

    case update(SessionId, Update, Info) of
        {ok, Code, _Hs, _Body} when Code == 200 orelse Code == 201 -> ok;
        {ok, Code, _Hs, Body} ->
            lager:error("remove_user_from_session failed [~p]: ~p", [Code, Body]),
            error;
        Error ->
            lager:error("remove_user_from_session failed: ~p", [Error]),
            error
    end,

    {noreply, State};

handle_cast({update_user, User}, #state{info=Info}=State) ->
    Json = ogonek_user:to_json(User),
    case replace(User#user.id, Json, Info) of
        {ok, 201, _Hs, _Body} -> ok;
        Error ->
            lager:error("failed to update user: ~p", [Error])
    end,
    {noreply, State};

handle_cast({planet_create, Planet}, #state{info=Info}=State) ->
    Json = ogonek_planet:to_json(Planet),
    insert(Json, Info),
    {noreply, State};

handle_cast({planet_update, Planet}, #state{info=Info}=State) ->
    Json = ogonek_planet:to_json(Planet),
    case replace(Planet#planet.id, Json, Info) of
        {ok, 201, _Hs, _Body} -> ok;
        Error ->
            lager:error("failed to update planet: ~p", [Error])
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

-spec get_info() -> db_info().
get_info() ->
    gen_server:call(?MODULE, get_info).


get_auth() ->
    case {os:getenv("COUCH_USER"), os:getenv("COUCH_PW")} of
        {false, _} -> [];
        {_, false} -> [];
        {User, Pw} -> [{basic_auth, {list_to_binary(User), list_to_binary(Pw)}}]
    end.


get_(Path, #db_info{host=Host, headers=Headers, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    ogonek_util:json_get(Target, Headers, Options).


get_by_id(Id, Info) ->
    get_by_id(?OGONEK_DB_NAME, Id, Info).

get_by_id(Db, Id, Info) ->
    Path = <<"/", Db/binary, "/", Id/binary>>,
    get_(Path, Info).


head_(Path, #db_info{host=Host, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    Result = hackney:head(Target, [], [], Options),

    lager:debug("HEAD [~s] ~p", [Target, Result]),
    Result.


put_(Path, Info) ->
    put_(Path, [], Info).

put_(Path, Payload, #db_info{host=Host, headers=Headers, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    Encoded = case Payload of
                  [] -> [];
                  P when is_binary(P) -> P;
                  Obj -> jiffy:encode(Obj)
              end,
    Result = hackney:put(Target, Headers, Encoded, Options),

    lager:debug("PUT [~s] ~p", [Target, Result]),
    Result.


post_(Path, Payload, #db_info{host=Host, headers=Headers, options=Options}) ->
    Target = <<Host/binary, Path/binary>>,
    ogonek_util:json_post(Target, Headers, Payload, Options).


check_status(Info) ->
    {ok, Code, _Hs, _Body} = get_(<<"/_up">>, Info),
    Code == 200.


exists(Path, Info) ->
    {ok, Code, _Hs} = head_(Path, Info),
    Code == 200.


db_exists(Db, Info) ->
    exists(<<"/", Db/binary>>, Info).


design_doc_exists(Db, Doc, Info) ->
    exists(<<"/", Db/binary, "/_design/", Doc/binary>>, Info).


design_create_if_not_exists(Name, Doc, Info) ->
    design_create_if_not_exists(?OGONEK_DB_NAME, Name, Doc, Info).


design_create_if_not_exists(Db, Name, Doc, Info) ->
    case design_doc_exists(Db, Name, Info) of
        true -> ok;
        false -> design_create(Db, Name, Doc, Info)
    end.


design_create(Db, Name, Doc, Info) ->
    Target = <<"/", Db/binary, "/_design/", Name/binary>>,
    case put_(Target, Doc, Info) of
        {ok, Code, _Hs, _Body} when Code == 201 orelse Code == 202 ->
            lager:info("successfully created design document '~s'", [Name]),
            ok;
        _Otherwise -> error
    end.


db_create(Db, Info) ->
    {ok, Code, _Hs, Body} = put_(<<"/", Db/binary>>, Info),
    case Code of
        201 ->
            lager:info("successfully created database '~s'", [Db]),
            ok;
        Error ->
            lager:error("failed to create database '~s' [~p]: ~p", [Db, Error, Body]),
            {error, Error}
    end.


db_create_if_not_exists(Db, Info) ->
    case db_exists(Db, Info) of
        true -> ok;
        false -> db_create(Db, Info)
    end.


insert(Doc, Info) ->
    insert(?OGONEK_DB_NAME, Doc, Info).

insert(Db, Doc, Info) ->
    case post_(<<"/", Db/binary>>, Doc, Info) of
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


update(Id, Func, Info) ->
    update(?OGONEK_DB_NAME, Id, Func, Info).

update(Db, Id, Func, Info) ->
    Path = <<"/", Db/binary, "/", Id/binary>>,

    case get_(Path, Info) of
        {ok, Code, _Hs, Body} ->
            Updated = Func(Code, Body),
            put_(Path, Updated, Info);
        Otherwise ->
            Otherwise
    end.


replace(Id, Entity, Info) ->
    replace(?OGONEK_DB_NAME, Id, Entity, Info).

replace(Db, Id, Entity, Info) ->
    Path = <<"/", Db/binary, "/", Id/binary>>,
    case get_rev(Path, Info) of
        {ok, Rev} ->
            WithRev = ogonek_util:replace_with(Entity, [{<<"_rev">>, Rev}]),
            put_(Path, WithRev, Info);
        error ->
            % we assume the document does not exist (yet)
            % let's try to PUT without a revision
            put_(Path, Entity, Info)
    end.


get_rev(Path, Info) ->
    case head_(Path, Info) of
        {ok, 200, Hs} ->
            case proplists:get_value(<<"ETag">>, Hs) of
                undefined -> error;
                QuotedRevision ->
                    [Revision] = binary:split(QuotedRevision, <<"\"">>, [global, trim_all]),
                    {ok, Revision}
            end;
        _Error -> error
    end.


prepare_headers(Headers) ->
    % TODO: filter out 'interesting' headers only?
    lists:foldl(fun({H, V}, Hs) ->
                    [{ogonek_util:lowercase(H), V} | Hs]
                end, [], Headers).


-spec parse_id_rev(tuple()) -> {ok, binary(), binary()} | {error, missing_id} | {error, missing_rev}.
parse_id_rev({Json}) ->
    Id = proplists:get_value(<<"id">>, Json),
    Rev = proplists:get_value(<<"rev">>, Json),
    case {Id, Rev} of
        {undefined, _} -> {error, missing_id};
        {_, undefined} -> {error, missing_rev};
        _ -> {ok, Id, Rev}
    end.


singleton_from_view(Design, View, Key, Info) ->
    singleton_from_view(?OGONEK_DB_NAME, Design, View, Key, Info).

singleton_from_view(Db, Design, View, Key, Info) ->
    JsonKey = jiffy:encode(Key),
    Target = <<"/", Db/binary, "/_design/", Design/binary,
               "/_view/", View/binary,
               "?key=", JsonKey/binary>>,

    case get_(Target, Info) of
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


list_from_view(Design, View, Skip, Limit, Info) ->
    list_from_view(?OGONEK_DB_NAME, Design, View, Skip, Limit, Info).

list_from_view(Db, Design, View, Skip, Limit, Info) ->
    Skip0 = list_to_binary(integer_to_list(Skip)),
    Limit0 = list_to_binary(integer_to_list(Limit)),
    Target = <<"/", Db/binary, "/_design/", Design/binary,
               "/_view/", View/binary,
               "?skip=", Skip0/binary,
               "&limit=", Limit0/binary>>,

    case get_(Target, Info) of
        {ok, 200, _Hs, Body} ->
            case ogonek_util:keys([<<"rows">>], Body) of
                [] -> [];
                [[]] -> [];
                [Results] when is_list(Results) ->
                    ValueKey = [<<"value">>],
                    lists:flatmap(fun(Elem) -> ogonek_util:keys(ValueKey, Elem) end, Results);
                _Otherwise -> []
            end;
        _Otherwise -> []
    end.


from_view(Design, View, Key, Info) ->
    from_view(?OGONEK_DB_NAME, Design, View, Key, Info).

from_view(Db, Design, View, Key, Info) ->
    JsonKey = jiffy:encode(Key),
    Target = <<"/", Db/binary, "/_design/", Design/binary,
               "/_view/", View/binary,
               "?key=", JsonKey/binary>>,

    case get_(Target, Info) of
        {ok, 200, _Hs, Body} ->
            case ogonek_util:keys([<<"rows">>], Body) of
                [] -> [];
                [[]] -> [];
                [Results] when is_list(Results) ->
                    ValueKey = [<<"value">>],
                    lists:flatmap(fun(Elem) -> ogonek_util:keys(ValueKey, Elem) end, Results);
                _Otherwise -> []
            end;
        _Otherwise -> []
    end.


with_id({Doc}, Id) ->
    with_id(Doc, Id);

with_id(Doc, Id) ->
    {[{<<"_id">>, Id} | Doc]}.
