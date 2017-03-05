module ServerApi exposing (..)

import Json.Decode as JsonD
import Json.Encode as JsonE
import Http

type alias Post =
    { id : Maybe Int
    , title : Maybe String
    , content : Maybe String
    }



baseUrl : String
baseUrl =
    "http://localhost:8081"


getPosts : (Result Http.Error (List Post) -> msg) -> Cmd msg
getPosts msg =
    Http.get (baseUrl ++ "/post/") postsDecoder
        |> Http.send msg

getPost : Int -> (Result Http.Error Post -> msg) -> Cmd msg
getPost id msg =
    Http.get (baseUrl ++ "/post/" ++ toString id) postDecoder
        |> Http.send msg

updatePost : Post -> (Result Http.Error Post -> msg) -> Cmd msg
updatePost post msg =
    Http.post (baseUrl ++ "/post/")
        (Http.stringBody "application/json" <| encodePost post) postDecoder
        |> Http.send msg

postsDecoder : JsonD.Decoder (List Post)
postsDecoder =
    JsonD.list postDecoder

postDecoder : JsonD.Decoder Post
postDecoder =
    JsonD.map3 Post
        (JsonD.field "postId" (JsonD.maybe JsonD.int))
        (JsonD.field "postTitle" (JsonD.maybe JsonD.string))
        (JsonD.field "postContent" (JsonD.maybe JsonD.string))

encodePost : Post -> String
encodePost post =
    JsonE.encode 0 <|
        JsonE.object
            [ ( "postId",
                case post.id of
                    Just postId -> JsonE.int postId
                    Nothing -> JsonE.null)
            , ("postTitle",
                case post.title of
                    Just postTitle -> JsonE.string postTitle
                    Nothing -> JsonE.null)
            , ("postContent",
                case post.content of
                    Just postContent -> JsonE.string postContent
                    Nothing -> JsonE.null)
            ]
