module PostTest exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag, class)

import Post
import TestUtils            exposing (testPost)


all : Test
all =
    describe "Post component"
        [ describe "testing HTML"
            [ fuzz string "a post displays content" <|
                \str ->
                    Post.view (testModel str) Nothing
                    |> Query.fromHtml
                    |> Query.has [ text str ]
            , test "a post displays 'edit' button when supplied with JWT" <|
                \() ->
                    Post.view (testModel "") (Just "")
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon-pencil" ]
                    |> Query.has [ tag "span" ]
            , test "a post displays 'delete' button when supplied with JWT" <|
                \() ->
                    Post.view (testModel "") (Just "")
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon-trash" ]
                    |> Query.has [ tag "span" ]
            , test "a post displays creation date" <|
                \() ->
                    Post.view (testModel "") (Just "")
                    |> Query.fromHtml
                    |> Query.has [ text "14 Apr 2017" ]
            , test "a post displays http link as a link" <|
                \() ->
                    Post.view
                        (testModel "text https://test.com more text")
                        Nothing
                    |> Query.fromHtml
                    |> Query.has [ tag "a" ]
            ]
        ]

testModel : String -> Post.Model
testModel postContent  = Post.Model (Just (testPost postContent)) Nothing
