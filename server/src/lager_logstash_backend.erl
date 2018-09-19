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

-module(lager_logstash_backend).

-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).


-record(state, {
          socket :: gen_udp:socket() | undefined,
          level :: non_neg_integer(),
          logstash_host :: string(),
          logstash_port :: inet:port_number(),
          logstash_address :: inet:ip_address() | undefined,
          metadata :: list()
         }).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(Params) ->
    Level = lager_util:level_to_num(proplists:get_value(level, Params, debug)),
    Host = proplists:get_value(logstash_host, Params, "localhost"),
    Port = proplists:get_value(logstash_port, Params, 9125),

    Metadata = proplists:get_value(metadata, Params, []) ++
    [{pid, [{encoding, process}]},
     {function, [{encoding, atom}]},
     {line, [{encoding, integer}]},
     {file, [{encoding, string}]},
     {module, [{encoding, atom}]},
     % http metadata
     {request, [{encoding, json}]},
     {response, [{encoding, json}]},
     {method, [{encoding, binary}]},
     {status_code, [{encoding, integer}]}
    ],

    {Socket, Address} =
    case inet:getaddr(Host, inet) of
        {ok, Addr} ->
            {ok, Sock} = gen_udp:open(0, [list]),
            {Sock, Addr};
        {error, _Err} ->
            {undefined, undefined}
    end,

    {ok, #state{socket=Socket,
                level=Level,
                logstash_host=Host,
                logstash_port=Port,
                logstash_address=Address,
                metadata=Metadata}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call({set_loglevel, Level}, State) ->
    NumLevel = lager_util:level_to_num(Level),
    {ok, ok, State#state{level=NumLevel}};

handle_call(get_loglevel, State) ->
    {ok, State#state.level, State};

handle_call(_Request, State) ->
    {ok, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(_Log, #state{socket=undefined}=State) ->
    {ok, State};

handle_event({log, Message}, #state{level=Level}=State) ->
    case lager_util:is_loggable(Message, Level, ?MODULE) of
        true ->
            Meta = metadata(lager_msg:metadata(Message), State#state.metadata),
            Encoded = encode_json_event(node(), Message, Meta),

            gen_udp:send(State#state.socket,
                         State#state.logstash_address,
                         State#state.logstash_port,
                         Encoded);
        false -> ok
    end,
    {ok, State};

handle_event(_Event, State) ->
  {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=undefined}) ->
    ok;
terminate(_Reason, #state{socket=S}) ->
    gen_udp:close(S),
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

encode_json_event(Node, Msg, Metadata) ->
    {Date, Time} = lager_msg:datetime(Msg),
    WithoutUTC = re:replace(Time, "(\\s+)UTC", "", [{return, list}]),
    DateTime = io_lib:format("~sT~sZ", [Date, WithoutUTC]),
    Severity = lager_msg:severity(Msg),
    SeverityValue = lager_msg:severity_as_int(Msg),
    Message = lager_msg:message(Msg),

    jiffy:encode({[{<<"level">>, Severity},
                   {<<"level_value">>, SeverityValue},
                   {<<"node">>, Node},
                   {<<"@timestamp">>, list_to_binary(DateTime)},
                   {<<"message">>, unicode:characters_to_binary(Message)},
                   {<<"type">>, <<"erlang">>}
                  ] ++ Metadata
                 }).


metadata(Metadata, ConfigMeta) ->
    Expanded = [{Name, Properties, proplists:get_value(Name, Metadata)} || {Name, Properties} <- ConfigMeta],
    [{Name, encode_value(Value, proplists:get_value(encoding, Properties))} || {Name, Properties, Value} <- Expanded, Value =/= undefined].


encode_value(Val, string) when is_list(Val) -> list_to_binary(Val);
encode_value(Val, string) when is_binary(Val) -> Val;
encode_value(Val, string) when is_atom(Val) -> Val;
encode_value(Val, binary) when is_list(Val) -> list_to_binary(Val);
encode_value(Val, binary) -> Val;
encode_value(Val, process) when is_pid(Val) -> list_to_binary(pid_to_list(Val));
encode_value(Val, process) when is_list(Val) -> list_to_binary(Val);
encode_value(Val, process) when is_atom(Val) -> Val;
encode_value(Val, integer) -> Val;
encode_value(Val, atom) -> Val;
encode_value(Val, json) -> Val;
encode_value(_Val, undefined) -> throw(encoding_error).
