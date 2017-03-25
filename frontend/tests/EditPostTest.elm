module EditPostTest exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, attribute, class)

import EditPost
import Util                 exposing (testPost, testPostEmpty)


all : Test
all =
    describe "EditPost component"
        [ describe "testing HTML"
            [ fuzz string "a post displays content" <|
                \str ->
                    EditPost.view
                        (EditPost.Model (Just (testPost str)) (Just (testPost str)))
                        ""
                    |> Query.fromHtml
                    |> Query.has [ attribute "value" str ]
            , test "a post displays title placeholder" <|
                \() ->
                    EditPost.view
                        (EditPost.Model
                            (Just testPostEmpty)
                            (Just testPostEmpty))
                        ""
                    |> Query.fromHtml
                    |> Query.find [ class "test-edit-post-title" ]
                    |> Query.has [ attribute "placeholder" "..type here the title of the post" ]
            , test "a post displays content placeholder" <|
                \() ->
                    EditPost.view
                        (EditPost.Model
                            (Just testPostEmpty)
                            (Just testPostEmpty))
                        ""
                    |> Query.fromHtml
                    |> Query.find [ class "test-edit-post-content" ]
                    |> Query.has [ attribute "placeholder" "..type here the content of the post" ]
            , test "a post displays 'save' button when client version differs from the server one" <|
                \() ->
                    EditPost.view
                        (EditPost.Model
                            (Just (testPost "post on the server"))
                            (Just (testPost "post on the client")))
                        ""
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon-floppy-disk" ]
                    |> Query.has [ attribute "title" "Save changes" ]
            ]
        ]

