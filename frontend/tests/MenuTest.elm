module MenuTest exposing (..)

import Test                 exposing (..)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (tag, class)

import Menu


all : Test
all =
    describe "Menu component"
        [ describe "testing HTML"
            [ test "menu has home button" <|
                \() ->
                    Menu.view Nothing
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon-home" ]
                    |> Query.has [ tag "span" ]
            , test "menu has login button when no JWT is provided" <|
                \() ->
                    Menu.view Nothing
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon-log-in" ]
                    |> Query.has [ tag "span" ]
            , test "menu has new-post button when a JWT is provided" <|
                \() ->
                    Menu.view (Just "")
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon-pencil" ]
                    |> Query.has [ tag "span" ]
            ]
        ]

