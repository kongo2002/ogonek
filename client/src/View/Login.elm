-- Copyright 2018 Gregor Uhlenheuer
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module View.Login exposing ( login )

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onSubmit )

import Types exposing (..)


login : Model -> List (Html Msg)
login model =
  let localAuth =
        div [ class "row" ]
        [ h3 [] [ text "local login" ]
        , Html.form [ onSubmit LocalLogin ]
          [ div [ class "row" ]
            [ label [ for "localUserInput" ] [ text "user" ]
            , input [ type_ "email", id "localUserInput", placeholder "user@email.com", onInput (FormContent "localUserInput") ] []
            ]
          , div [ class "row" ]
            [ label [ for "localPasswordInput" ] [ text "password" ]
            , input [ type_ "password", id "localPasswordInput", placeholder "password", onInput (FormContent "localPasswordInput") ] []
            ]
          , div [ class "row" ]
            [ input [ class "button-primary", type_ "submit", value "login" ] []
            ]
          ]
        ]

      fromAuth auth =
        let provider = auth.provider
            login = auth.loginUrl
            viaProvider =
              div [ class "row" ]
              [ h3 [] [ text ("via " ++ provider) ]
              , p []
                [ a [ class "button button-primary", href login ] [ text "login" ]
                ]
              ]
        in if provider == "local" then localAuth else viaProvider

      providers =
        if Dict.isEmpty model.authInfo then
          -- fallback to local auth in case no providers given at all
          [ localAuth ]
        else Dict.values model.authInfo |> List.map fromAuth
  in [ div [ class "row" ]
       [ h2 [] [ text "login" ] ]
     ] ++ providers


-- vim: et sw=2 sts=2
