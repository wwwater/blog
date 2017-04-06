module PostTest exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, tag, class)

import Post
import Util                 exposing (testPost)


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
                    |> Query.has [ tag "a" ]
            , test "a post displays 'delete' button when supplied with JWT" <|
                \() ->
                    Post.view (testModel "") (Just "")
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon-trash" ]
                    |> Query.has [ tag "span" ]
            ]
        ]

testModel : String -> Post.Model
testModel postContent  = Post.Model (Just (testPost postContent)) Nothing
