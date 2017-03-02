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


routeParser : Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map PostsPage (s "")
        , UrlParser.map PostPage (s "post" </> int)
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



navigate : Route -> Cmd msg
navigate route =
    Navigation.newUrl (encode route)


linkTo : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo route attrs content =
    a ((linkAttrs route) ++ attrs) content


linkAttrs : Route -> List (Attribute msg)
linkAttrs route =
    let
        path =
            encode route
    in
        [ href path
        , attribute "data-navigate" path
        ]


catchNavigationClicks : (String -> msg) -> Attribute msg
catchNavigationClicks tagger =
    onWithOptions "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.map tagger (Json.at [ "target" ] pathDecoder))


pathDecoder : Json.Decoder String
pathDecoder =
    Json.oneOf
        [ Json.at [ "data-navigate" ] Json.string
        , Json.at [ "parentElement" ] (Json.lazy (\_ -> pathDecoder))
        , Json.fail "no path found for click"
        ]
