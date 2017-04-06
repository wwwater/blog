module Menu exposing (Model, Msg, init, view, update, mountCmd)

import ServerApi exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Routes

type alias Model = {}


type Msg
    = GoToPosts
    | GoToLogin
    | GoToNewPost


init : Model
init = Model


mountCmd : Cmd Msg
mountCmd = Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        GoToPosts ->
            (model, Routes.navigate Routes.PostsPage)

        GoToLogin ->
            (model, Routes.navigate Routes.LoginPage)

        GoToNewPost ->
            (model, Routes.navigate Routes.NewPostPage)


------ VIEW ------
iconStyle : Attribute msg
iconStyle = style [
      ("color", "#fff")
    , ("margin-left", "24px")
    , ("cursor", "pointer")
    , ("box-shadow", "2px 2px 15px 1px #333")
    , ("padding", "10px")
    , ("border-radius", "15px")
    , ("background", "#555")
    ]

loginOrNewPostIcon : Maybe Jwt -> Html Msg
loginOrNewPostIcon jwt =
    case jwt of
        Just _ -> a [ iconStyle
                      , class "glyphicon glyphicon-pencil"
                      , onClick GoToNewPost
                      ] []
        Nothing -> a [ iconStyle
                      , class "glyphicon glyphicon-log-in"
                      , onClick GoToLogin
                      ] []

view : Maybe Jwt -> Html Msg
view jwt = div [ style [ ("background-color", "#777")
                         , ("display", "flex")
                         , ("justify-content", "flex-end")
                         , ("font-size", "16px")
                         , ("margin-bottom", "-16px")
                         , ("padding", "16px 16px 0 16px") ] ]
                 [ loginOrNewPostIcon jwt
                 , a
                     [ iconStyle
                     , class "glyphicon glyphicon-home"
                     , onClick GoToPosts
                     ] []
                 ]
