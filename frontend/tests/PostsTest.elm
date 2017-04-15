module PostsTest exposing (..)

import Test                 exposing (..)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag)

import Posts
import TestUtils            exposing (testPostEmpty, testPost)


all : Test
all =
    describe "Posts component"
        [ describe "testing HTML"
            [ test "a post has an 'empty' title" <|
                \() ->
                    Posts.view (Posts.Model [testPostEmpty] [])
                    |> Query.fromHtml
                    |> Query.find [ tag "h2" ]
                    |> Query.has [ text "<no title>" ]
            , test "a post displays title" <|
                \() ->
                    Posts.view (Posts.Model [testPost ""] [])
                    |> Query.fromHtml
                    |> Query.has [ text "Nice" ]
            , test "a post displays content" <|
                \() ->
                    Posts.view (Posts.Model [testPost "sun and wind"] [])
                    |> Query.fromHtml
                    |> Query.has [ text "sun" ]
            , test "a post displays creating time" <|
                \() ->
                    Posts.view (Posts.Model [testPost ""] [])
                    |> Query.fromHtml
                    |> Query.has [ text "14 Apr 2017" ]
            , test "a post displays http link as a link" <|
                \() ->
                    Posts.view (Posts.Model
                        [testPost "text https://test.com more text"] [])
                    |> Query.fromHtml
                    |> Query.has [ tag "a" ]
            ]
        ]

