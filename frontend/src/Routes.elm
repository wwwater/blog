module Routes exposing (..)

import UrlParser exposing (Parser, (</>), int, oneOf, s)
import Navigation exposing (Location)
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json


type Route
    = PostsPage
    | PostPage Int
    | EditPostPage Int
    | NewPostPage
    | LoginPage


routeParser : Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map PostsPage (s "")
        , UrlParser.map PostPage (s "post" </> int)
        , UrlParser.map EditPostPage (s "edit" </> int)
        , UrlParser.map NewPostPage (s "new")
        , UrlParser.map LoginPage (s "login")
        ]


decode : Location -> Maybe Route
decode location =
    UrlParser.parsePath routeParser location



encode : Route -> String
encode route =
    case route of

        PostsPage ->
            "/"

        PostPage i ->
            "/post/" ++ toString i

        EditPostPage i ->
            "/edit/" ++ toString i

        NewPostPage ->
            "/new"

        LoginPage ->
            "/login"



navigate : Route -> Cmd msg
navigate route =
    Navigation.newUrl (encode route)

