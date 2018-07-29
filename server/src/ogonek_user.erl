-module(ogonek_user).

-include("ogonek.hrl").

-export([from_json/1,
         to_json/1]).


from_json(UserJson) ->
    Keys = [<<"_id">>, <<"provider">>, <<"pid">>, <<"email">>, <<"name">>, <<"img">>],

    case ogonek_util:keys(Keys, UserJson) of
        [Id, Provider, Pid, Email, Name, Img] ->
            {ok, #user{id=Id,
                       provider=Provider,
                       provider_id=Pid,
                       email=Email,
                       name=Name,
                       img=Img}};
        _Otherwise ->
            {error, invalid_user}
    end.


to_json(#user{}=User) ->
    ogonek_util:doc(<<"user">>,
                    {[{<<"_id">>, User#user.id},
                      {<<"provider">>, User#user.provider},
                      {<<"pid">>, User#user.provider_id},
                      {<<"email">>, User#user.email},
                      {<<"name">>, User#user.name},
                      {<<"img">>, User#user.img}
                     ]}).
