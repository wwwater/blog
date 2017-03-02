module ServerApi exposing (..)

import Json.Decode as JsonD
import Json.Encode as JsonE
import Http

type alias Post =
    { id : Int
    , title : String
    , content : String
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

postsDecoder : JsonD.Decoder (List Post)
postsDecoder =
    JsonD.list postDecoder

postDecoder : JsonD.Decoder Post
postDecoder =
    JsonD.map3 Post
        (JsonD.field "postId" JsonD.int)
        (JsonD.field "postTitle" JsonD.string)
        (JsonD.field "postContent" JsonD.string)

