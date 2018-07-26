-module(ogonek_util).

-export([lowercase/1,
         uppercase/1,
         parse_json/1
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
