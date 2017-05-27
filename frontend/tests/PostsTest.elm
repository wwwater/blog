module PostsTest exposing (..)

import Test                 exposing (..)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag, class)

import Posts
import TestUtils            exposing (testPostEmpty, testPost)


all : Test
all =
    describe "Posts component"
        [ describe "testing HTML"
            [ test "a post has an 'empty' title" <|
                \() ->
                    Posts.view
                        (Posts.Model [testPostEmpty] Nothing 1 False)
                        Nothing
                    |> Query.fromHtml
                    |> Query.find [ tag "h2" ]
                    |> Query.has [ text "<no title>" ]
            , test "a post displays title" <|
                \() ->
                    Posts.view
                        (Posts.Model [testPost ""] Nothing 1 False)
                        Nothing
                    |> Query.fromHtml
                    |> Query.has [ text "Nice" ]
            , test "a post displays content" <|
                \() ->
                    Posts.view
                        (Posts.Model [testPost "sun and wind"] Nothing 1 False)
                        Nothing
                    |> Query.fromHtml
                    |> Query.has [ text "sun" ]
            , test "a post displays creating time" <|
                \() ->
                    Posts.view
                        (Posts.Model [testPost ""] Nothing 1 False)
                        Nothing
                    |> Query.fromHtml
                    |> Query.has [ text "14 Apr 2017" ]
            , test "a post displays http link as a link" <|
                \() ->
                    Posts.view
                        (Posts.Model [testPost "text https://test.com more text"] Nothing 1 False)
                        Nothing
                    |> Query.fromHtml
                    |> Query.has [ tag "a" ]
            , test "the show-more-posts button is displayed" <|
                \() ->
                    Posts.view
                        (Posts.Model [testPost ""] Nothing 1 True)
                        Nothing
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon" ]
                    |> Query.has [ tag "span" ]
            ]
        ]

