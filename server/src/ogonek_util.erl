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

-module(ogonek_util).

-include("ogonek.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([trim/1,
         lowercase/1,
         uppercase/1,
         parse_json/1,
         json_get/1,
         json_get/2,
         json_get/3,
         json_delete/2,
         json_delete/3,
         json_delete/4,
         json_post/1,
         json_post/2,
         json_post/3,
         json_post/4,
         json_put/1,
         json_put/2,
         json_put/3,
         json_put/4,
         keys/2,
         path/2,
         replace_with/2,
         doc/2,
         remove_key/2,
         choose_random/1,
         if_defined/2,
         with_id/2,
         seconds_since/1,
         seconds_since/2,
         now8601/0
        ]).


-spec trim(binary()) -> binary().
trim(Binary) ->
    % TODO: replace with 'string:trim' for OTP >= 20
    re:replace(Binary, "^\\s+|\\s+$", "", [{return, binary}, global]).


-spec now8601() -> timestamp().
now8601() ->
    iso8601:format(calendar:universal_time()).


-spec lowercase(string() | binary()) -> string() | binary().
lowercase(Str) when is_list(Str) ->
    string:to_lower(Str);

lowercase(Bin) when is_binary(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).


-spec uppercase(string() | binary()) -> string() | binary().
uppercase(Str) when is_list(Str) ->
    string:to_upper(Str);

uppercase(Bin) when is_binary(Bin) ->
    list_to_binary(string:to_upper(binary_to_list(Bin))).


-spec parse_json(binary()) -> {ok, any()} | {error, malformed_json}.
parse_json(Body) ->
    try
        Json = jiffy:decode(Body),
        {ok, Json}
    catch
        _Error -> {error, malformed_json}
    end.


-spec keys([term()], json_doc()) -> list().
keys(Keys, {Json}) -> keys(Keys, Json);
keys(Keys, Json) when is_list(Json) ->
    lists:foldr(fun({Key, Default}, Res) ->
                        Value = proplists:get_value(Key, Json, Default),
                        [Value | Res];
                   (Key, Res) ->
                        case proplists:get_value(Key, Json) of
                            undefined -> Res;
                            Found -> [Found | Res]
                        end
                end, [], Keys);

keys(_Keys, _Json) -> [].


-spec path([term()], json_doc()) -> term() | undefined.
path([], Json) -> Json;
path(Path, {Json}) -> path(Path, Json);
path([Part | Path], Json) when is_list(Json) ->
    case proplists:get_value(Part, Json) of
        undefined -> undefined;
        Result -> path(Path, Result)
    end;
path(_Path, _Json) -> undefined.


-spec replace_with(json_doc(), json_props()) -> json_props().
replace_with({PList}, Values) ->
    {replace_with(PList, Values)};

replace_with(PList, Values) when is_list(PList) ->
    Combined = Values ++ PList,
    lists:usort(fun({K1, _}, {K2, _}) -> K1 =< K2 end, Combined).


remove_key({PList}, Key) ->
    {remove_key(PList, Key)};

remove_key(PList, Key) ->
    lists:keydelete(Key, 1, PList).


json_delete(Target, Revision) ->
    json_delete(Target, Revision, []).


json_delete(Target, Revision, Headers) ->
    DefaultOpts = [{pool, default}, with_body],
    json_delete(Target, Revision, Headers, DefaultOpts).


json_delete(Target, Revision, Headers, Options) ->
    Headers0 = [{<<"If-Match">>, Revision} | Headers],
    case hackney:delete(Target, Headers0, [], Options) of
        {ok, Code, Hs, Body} = Res ->
            case parse_json(Body) of
                {ok, Json} ->
                    lager:debug([{response, Json}, {status_code, Code}, {method, <<"DELETE">>}], "DELETE ~s", [Target]),
                    {ok, Code, Hs, Json};
                {error, malformed_json} ->
                    lager:warning([{status_code, Code}, {method, <<"DELETE">>}], "DELETE ~s - malformed_json", [Target]),
                    Res
            end;
        Otherwise ->
            lager:warning([{method, <<"DELETE">>}], "DELETE [~s] ~p", [Target, Otherwise]),
            Otherwise
    end.


json_get(Target) ->
    json_get(Target, []).


json_get(Target, Headers) ->
    DefaultOpts = [{pool, default}, with_body],
    json_get(Target, Headers, DefaultOpts).


json_get(Target, Headers, Options) ->
    case hackney:get(Target, Headers, [], Options) of
        {ok, Code, Hs, Body} = Res ->
            case parse_json(Body) of
                {ok, Json} ->
                    lager:debug([{response, Json}, {status_code, Code}, {method, <<"GET">>}], "GET ~s", [Target]),
                    {ok, Code, Hs, Json};
                {error, malformed_json} ->
                    lager:warning([{status_code, Code}, {method, <<"GET">>}], "GET ~s - malformed_json", [Target]),
                    Res
            end;
        Otherwise ->
            lager:warning([{method, <<"GET">>}], "GET [~s] ~p", [Target, Otherwise]),
            Otherwise
    end.


json_post(Target) ->
    json_post(Target, []).


json_post(Target, Headers) ->
    json_post(Target, Headers, []).


json_post(Target, Headers, Payload) ->
    DefaultOpts = [{pool, default}, with_body],
    json_post(Target, Headers, Payload, DefaultOpts).


json_post(Target, Headers, Payload, Options) ->
    {Meta, JsonPayload} = case Payload of
                              [] -> {[], []};
                              Bin when is_binary(Bin) -> {[], Bin};
                              Obj -> {[{request, Obj}], jiffy:encode(Obj)}
                          end,
    Meta0 = [{method, <<"POST">>} | Meta],

    case hackney:post(Target, Headers, JsonPayload, Options) of
        {ok, Code, Hs, Body} = Res ->
            case parse_json(Body) of
                {ok, Json} ->
                    LogMeta = [{response, Json}, {status_code, Code}] ++ Meta0,
                    lager:debug(LogMeta, "POST ~s", [Target]),
                    {ok, Code, Hs, Json};
                {error, malformed_json} ->
                    LogMeta = [{status_code, Code}] ++ Meta0,
                    lager:warning(LogMeta, "POST ~s - malformed_json", [Target]),
                    Res
            end;
        Otherwise ->
            lager:warning(Meta0, "POST [~s] ~p", [Target, Otherwise]),
            Otherwise
    end.


json_put(Target) ->
    json_put(Target, []).


json_put(Target, Headers) ->
    json_put(Target, Headers, []).


json_put(Target, Headers, Payload) ->
    DefaultOpts = [{pool, default}, with_body],
    json_put(Target, Headers, Payload, DefaultOpts).


json_put(Target, Headers, Payload, Options) ->
    {Meta, JsonPayload} = case Payload of
                              [] -> {[], []};
                              Bin when is_binary(Bin) -> {[], Bin};
                              Obj -> {[{request, Obj}], jiffy:encode(Obj)}
                          end,
    Meta0 = [{method, <<"PUT">>} | Meta],

    case hackney:put(Target, Headers, JsonPayload, Options) of
        {ok, Code, Hs, Body} = Res ->
            case parse_json(Body) of
                {ok, Json} ->
                    LogMeta = [{response, Json}, {status_code, Code}] ++ Meta0,
                    lager:debug(LogMeta, "PUT ~s", [Target]),
                    {ok, Code, Hs, Json};
                {error, malformed_json} ->
                    LogMeta = [{status_code, Code}] ++ Meta0,
                    lager:warning(LogMeta, "PUT ~s - malformed_json", [Target]),
                    Res
            end;
        Otherwise ->
            lager:warning(Meta0, "PUT [~s] ~p", [Target, Otherwise]),
            Otherwise
    end.


-spec doc(binary(), json_doc()) -> {json_props()}.
doc(DocType, {Vs}) ->
    doc(DocType, Vs);

doc(DocType, Values) when is_list(Values) ->
    {[{?MSG_TYPE, DocType} | Values]}.


-spec seconds_since(Timestamp :: binary()) -> integer().
seconds_since(Timestamp) ->
    seconds_since(Timestamp, ogonek_util:now8601()).


-spec seconds_since(Timestamp :: timestamp(), RelativeTo :: timestamp()) -> integer().
seconds_since(Timestamp, RelativeTo) ->
    RelativeTime = iso8601:parse(RelativeTo),
    Since = iso8601:parse(Timestamp),
    abs(calendar:datetime_to_gregorian_seconds(RelativeTime) - calendar:datetime_to_gregorian_seconds(Since)).


-spec choose_random(nonempty_list()) -> term().
choose_random([Singleton]) -> Singleton;
choose_random(Candidates) ->
    Idx = rand:uniform(length(Candidates)),
    lists:nth(Idx, Candidates).


-spec if_defined(binary(), term()) -> list().
if_defined(_Key, undefined) -> [];
if_defined(Key, Value) -> [{Key, Value}].


-spec with_id(undefined | binary() | bson:objectid(), map()) -> map().
with_id(undefined, Doc) -> Doc;
with_id(Id, Doc) ->
    maps:put(<<"_id">>, ogonek_mongo:to_id(Id), Doc).


%%
%% TESTS
%%

-ifdef(TEST).

doc_test_() ->
    [?_assertEqual({[{?MSG_TYPE, <<"test">>}, {<<"foo">>, true}]},
                  doc(<<"test">>, [{<<"foo">>, true}])),
     ?_assertEqual({[{?MSG_TYPE, <<"test">>}, {<<"foo">>, true}]},
                  doc(<<"test">>, {[{<<"foo">>, true}]}))
    ].

path_test_() ->
    [?_assertEqual(undefined, path([<<"foo">>], {})),
     ?_assertEqual(undefined, path([<<"foo">>], [])),
     ?_assertEqual(undefined, path([<<"foo">>], true)),
     ?_assertEqual(true, path([<<"foo">>], [{<<"foo">>, true}])),
     ?_assertEqual(true, path([<<"foo">>], {[{<<"foo">>, true}]})),
     ?_assertEqual(true, path([<<"foo">>, <<"bar">>], [{<<"foo">>, {[{<<"bar">>, true}]}}])),
     ?_assertEqual(true, path([<<"foo">>, <<"bar">>], {[{<<"foo">>, {[{<<"bar">>, true}]}}]}))
    ].

keys_test_() ->
    [?_assertEqual([], keys([<<"foo">>], {})),
     ?_assertEqual([], keys([<<"foo">>], [])),
     ?_assertEqual([], keys([<<"foo">>], true)),
     ?_assertEqual([true], keys([<<"foo">>], [{<<"foo">>, true}])),
     ?_assertEqual([true], keys([<<"foo">>], {[{<<"foo">>, true}]})),
     ?_assertEqual([{[{<<"bar">>, true}]}], keys([<<"foo">>, <<"bar">>], [{<<"foo">>, {[{<<"bar">>, true}]}}])),
     ?_assertEqual([{[{<<"bar">>, true}]}], keys([<<"foo">>, <<"bar">>], {[{<<"foo">>, {[{<<"bar">>, true}]}}]})),
     ?_assertEqual([true, false], keys([<<"foo">>, <<"bar">>], [{<<"bar">>, false}, {<<"foo">>, true}]))
    ].

lowercase_test_() ->
    [?_assertEqual("foo", lowercase("Foo")),
     ?_assertEqual("foo", lowercase("foo")),
     ?_assertEqual("", lowercase("")),
     ?_assertEqual(<<"foo bar">>, lowercase(<<"Foo BAR">>)),
     ?_assertEqual(<<"foo bar ">>, lowercase(<<"foo bar ">>)),
     ?_assertEqual(<<>>, lowercase(<<>>))
    ].

uppercase_test_() ->
    [?_assertEqual("FOO", uppercase("Foo")),
     ?_assertEqual("FOO", uppercase("foo")),
     ?_assertEqual("", uppercase("")),
     ?_assertEqual(<<"FOO BAR">>, uppercase(<<"Foo BAR">>)),
     ?_assertEqual(<<"FOO BAR ">>, uppercase(<<"foo bar ">>)),
     ?_assertEqual(<<>>, uppercase(<<>>))
    ].

replace_with_test_() ->
    [?_assertEqual([], replace_with([], [])),
     ?_assertEqual([{<<"id">>, <<"1">>}], replace_with([], [{<<"id">>, <<"1">>}])),
     ?_assertEqual([{<<"id">>, <<"1">>}], replace_with([{<<"id">>, <<"0">>}], [{<<"id">>, <<"1">>}])),
     ?_assertEqual([{<<"bar">>, <<"2">>}, {<<"id">>, <<"1">>}],
                   replace_with([{<<"id">>, <<"0">>}], [{<<"id">>, <<"1">>}, {<<"bar">>, <<"2">>}]))
    ].

choose_random_test_() ->
    [?_assertEqual(1, choose_random([1])),
     ?_assertEqual(true, lists:member(choose_random([1, 2, 3]), [1, 2, 3])),
     ?_assertEqual(true, lists:member(choose_random([1, 2, 3]), [1, 2, 3])),
     ?_assertEqual(true, lists:member(choose_random([1, 2, 3]), [1, 2, 3])),
     ?_assertEqual(true, lists:member(choose_random([1, 2, 3]), [1, 2, 3]))
    ].

trim_test_() ->
    [?_assertEqual(<<"">>, trim(<<"">>)),
     ?_assertEqual(<<"foo">>, trim(<<"foo">>)),
     ?_assertEqual(<<"foo">>, trim(<<"  foo">>)),
     ?_assertEqual(<<"foo">>, trim(<<"foo   ">>)),
     ?_assertEqual(<<"foo">>, trim(<<"  foo   ">>))
    ].

-endif.
