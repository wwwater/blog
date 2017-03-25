module LoginTest exposing (..)

import Test                 exposing (..)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (attribute, tag, text)

import Login
import ServerApi


all : Test
all =
    describe "Login component"
        [ describe "testing HTML"
            [ test "has username input field" <|
                \() ->
                    Login.view testLoginModel
                    |> Query.fromHtml
                    |> Query.find [ attribute "type" "text" ]
                    |> Query.has [ attribute "value" "user" ]
            , test "has password input field" <|
                \() ->
                    Login.view testLoginModel
                    |> Query.fromHtml
                    |> Query.find [ attribute "type" "password" ]
                    |> Query.has [ attribute "value" "pass" ]
            , test "has submit button" <|
                \() ->
                    Login.view testLoginModel
                    |> Query.fromHtml
                    |> Query.find [ tag "button" ]
                    |> Query.has [ text "Login" ]
            ]
        ]

testLoginModel : Login.Model
testLoginModel = Login.Model
    (ServerApi.Credentials "user" "pass")
    Nothing
