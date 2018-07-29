-module(ogonek_util).

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
         keys/2
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


keys(Keys, {Json}) ->
    keys(Keys, Json);

keys(Keys, Json) when is_list(Json) ->
    lists:foldr(fun(Key, Res) ->
                        case proplists:get_value(Key, Json) of
                            undefined -> Res;
                            Found -> [Found | Res]
                        end
                end, [], Keys);

keys(_Keys, _Json) -> [].


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