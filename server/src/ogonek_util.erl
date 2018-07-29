-module(ogonek_util).

-export([lowercase/1,
         uppercase/1,
         parse_json/1,
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
