module ServerApi exposing (..)

import Json.Decode as JsonD
import Json.Encode as JsonE
import Http

type alias Post =
    { id : Maybe Int
    , title : Maybe String
    , content : Maybe String
    , createdAt : Maybe Int
    , published : Maybe Bool
    }

type alias Credentials =
    { username : String
    , password : String
    }

type alias Jwt = String


baseUrl : String
baseUrl =
    "http://localhost:8081"


getPosts : Maybe Int -> Maybe Jwt -> (Result Http.Error (List Post) -> msg) -> Cmd msg
getPosts maybeOffset maybeJwt msg =
    Http.request
        { method = "GET"
        , headers = case maybeJwt of
            Just jwt -> [ Http.header "jwt" jwt ]
            Nothing -> []
        , url = baseUrl ++ "/post?offset=" ++
            (toString (Maybe.withDefault 0 maybeOffset))
        , body = Http.emptyBody
        , expect = Http.expectJson postsDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg

getPost : Int -> Maybe Jwt -> (Result Http.Error Post -> msg) -> Cmd msg
getPost id maybeJwt msg =
    Http.request
        { method = "GET"
        , headers = case maybeJwt of
            Just jwt -> [ Http.header "jwt" jwt ]
            Nothing -> []
        , url = baseUrl ++ "/post/" ++ toString id
        , body = Http.emptyBody
        , expect = Http.expectJson postDecoder
        , timeout = Nothing
        , withCredentials = False
        }
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

publishPost : Int -> Jwt -> (Result Http.Error Post -> msg) -> Cmd msg
publishPost postId jwt msg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "jwt" jwt ]
        , url = baseUrl ++ "/post/" ++ toString postId ++ "/publish"
        , body = Http.emptyBody
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
    JsonD.map5 Post
        (JsonD.field "postId" (JsonD.maybe JsonD.int))
        (JsonD.field "postTitle" (JsonD.maybe JsonD.string))
        (JsonD.field "postContent" (JsonD.maybe JsonD.string))
        (JsonD.field "createdAt" (JsonD.maybe JsonD.int))
        (JsonD.field "published" (JsonD.maybe JsonD.bool))

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
