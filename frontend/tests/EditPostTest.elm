module EditPostTest exposing (..)

import Test                 exposing (..)
import Fuzz                 exposing (string)
import Expect               exposing (equal)
import Test.Html.Query      as Query
import Test.Html.Selector   exposing (text, attribute, class, tag)

import Http                 exposing (Error(..), Response)
import Dict                 exposing (empty)

import EditPost
import Util                 exposing (testPost, testPostEmpty)



all : Test
all =
    describe "EditPost component"
        [ describe "testing HTML"
            [ fuzz string "a post displays content" <|
                \str ->
                    EditPost.view
                        (EditPost.Model
                            (Just (testPost str))
                            (Just (testPost str))
                            Nothing)
                        ""
                    |> Query.fromHtml
                    |> Query.has [ attribute "value" str ]
            , test "a post displays title placeholder" <|
                \() ->
                    EditPost.view
                        (EditPost.Model
                            (Just testPostEmpty)
                            (Just testPostEmpty)
                            Nothing)
                        ""
                    |> Query.fromHtml
                    |> Query.find [ class "test-edit-post-title" ]
                    |> Query.has [ attribute "placeholder" "..type here the title of the post" ]
            , test "a post displays content placeholder" <|
                \() ->
                    EditPost.view
                        (EditPost.Model
                            (Just testPostEmpty)
                            (Just testPostEmpty)
                            Nothing)
                        ""
                    |> Query.fromHtml
                    |> Query.find [ class "test-edit-post-content" ]
                    |> Query.has [ attribute "placeholder" "..type here the content of the post" ]
            , test "a post displays 'save' button when client version differs from the server one" <|
                \() ->
                    EditPost.view
                        (EditPost.Model
                            (Just (testPost "post on the server"))
                            (Just (testPost "post on the client"))
                            Nothing)
                        ""
                    |> Query.fromHtml
                    |> Query.find [ class "glyphicon-floppy-disk" ]
                    |> Query.has [ attribute "title" "Save changes" ]
            , test "page displays error message" <|
                \() ->
                    EditPost.view
                        (EditPost.Model
                            Nothing
                            Nothing
                            (Just "I am an error"))
                        ""
                    |> Query.fromHtml
                    |> Query.find [ tag "h2" ]
                    |> Query.has [ text "I am an error" ]
            ]
        , describe "test component's update function"
            [ test "a request failed" <|
                \() ->
                    EditPost.update
                        (EditPost.HandlePostRetrieved (Result.Err response401))
                        (EditPost.Model Nothing Nothing Nothing)
                    |> Expect.equal (
                          (EditPost.Model
                            Nothing
                            Nothing
                            (Just "You shall not pass"))
                        , Cmd.none )

            ]
        ]

response401 : Http.Error
response401 = Http.BadStatus (
    Http.Response
        ""
        {code = 401, message = ""}
        Dict.empty
        "You shall not pass"
    )

