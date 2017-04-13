module ServerApi exposing (..)

import Json.Decode as JsonD
import Json.Encode as JsonE
import Http

type alias Post =
    { id : Maybe Int
    , title : Maybe String
    , content : Maybe String
    }

type alias Credentials =
    { username : String
    , password : String
    }

type alias Jwt = String


baseUrl : String
baseUrl =
    "http://46.101.142.224:8081"


getPosts : (Result Http.Error (List Post) -> msg) -> Cmd msg
getPosts msg =
    Http.get (baseUrl ++ "/post/") postsDecoder
        |> Http.send msg

getPost : Int -> (Result Http.Error Post -> msg) -> Cmd msg
getPost id msg =
    Http.get (baseUrl ++ "/post/" ++ toString id) postDecoder
        |> Http.send msg

getJwt : Credentials -> (Result Http.Error Jwt -> msg) -> Cmd msg
getJwt credentials msg =
    Http.post (baseUrl ++ "/jwt")
        (Http.stringBody "application/json" <| encodeCredentials credentials)
        jwtDecoder
        |> Http.send msg

updatePost : Post -> Jwt -> (Result Http.Error Post -> msg) -> Cmd msg
updatePost post jwt msg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "jwt" jwt ]
        , url = baseUrl ++ "/post/"
        , body = Http.stringBody "application/json" <| encodePost post
        , expect = Http.expectJson postDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg

deletePost : Int -> Jwt -> (Result Http.Error String -> msg) -> Cmd msg
deletePost postId jwt msg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "jwt" jwt ]
        , url = baseUrl ++ "/post/" ++ (toString postId)
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
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

encodeCredentials : Credentials -> String
encodeCredentials credentials =
    JsonE.encode 0 <|
        JsonE.object
            [ ("username", JsonE.string credentials.username)
            , ("password", JsonE.string credentials.password)
            ]

jwtDecoder : JsonD.Decoder Jwt
jwtDecoder = JsonD.field "token" JsonD.string
