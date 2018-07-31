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

-export([lowercase/1,
         uppercase/1,
         parse_json/1,
         json_get/1,
         json_get/2,
         json_get/3,
         json_post/1,
         json_post/2,
         json_post/3,
         json_post/4,
         keys/2,
         path/2,
         replace_with/2,
         doc/2
        ]).


lowercase(Str) when is_list(Str) ->
    string:to_lower(Str);

lowercase(Bin) when is_binary(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).


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


path([], Json) -> Json;
path(Path, {Json}) -> path(Path, Json);
path([Part | Path], Json) when is_list(Json) ->
    case proplists:get_value(Part, Json) of
        undefined -> undefined;
        Result -> path(Path, Result)
    end;
path(_Path, _Json) -> undefined.


replace_with(PList, Values) ->
    Combined = Values ++ PList,
    lists:usort(fun({K1, _}, {K2, _}) -> K1 =< K2 end, Combined).


json_get(Target) ->
    json_get(Target, []).


json_get(Target, Headers) ->
    DefaultOpts = [{pool, default}, with_body],
    json_get(Target, Headers, DefaultOpts).


json_get(Target, Headers, Options) ->
    Result = case hackney:get(Target, Headers, [], Options) of
                 {ok, Code, Hs, Body} = Res ->
                     case parse_json(Body) of
                         {ok, Json} -> {ok, Code, Hs, Json};
                         _Otherwise -> Res
                     end;
                 Otherwise -> Otherwise
             end,

    lager:debug("GET: ~p", [Result]),
    Result.


json_post(Target) ->
    json_post(Target, []).


json_post(Target, Headers) ->
    json_post(Target, Headers, []).


json_post(Target, Headers, Payload) ->
    DefaultOpts = [{pool, default}, with_body],
    json_post(Target, Headers, Payload, DefaultOpts).


json_post(Target, Headers, Payload, Options) ->
    JsonPayload = case Payload of
                      [] -> [];
                      Bin when is_binary(Bin) -> Bin;
                      Obj -> jiffy:encode(Obj)
                  end,
    Result = case hackney:post(Target, Headers, JsonPayload, Options) of
                 {ok, Code, Hs, Body} = Res ->
                     case parse_json(Body) of
                         {ok, Json} -> {ok, Code, Hs, Json};
                         _Otherwise -> Res
                     end;
                 Otherwise -> Otherwise
             end,

    lager:debug("POST: ~p", [Result]),
    Result.


doc(DocType, {Vs}) ->
    doc(DocType, Vs);

doc(DocType, Values) when is_list(Values) ->
    {[{?MSG_TYPE, DocType} | Values]}.


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

-endif.
