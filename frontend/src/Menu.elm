module Menu             exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( style, class, title )
import Html.Events      exposing ( onClick )

import Routes
import ServerApi        exposing (..)
import Styles           exposing (..)


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



loginOrNewPostIcon : Maybe Jwt -> Html Msg
loginOrNewPostIcon jwt =
    case jwt of
        Just _ -> span [ iconStyle
                       , class "glyphicon glyphicon-pencil"
                       , title "New post"
                       , onClick GoToNewPost
                       ] []
        Nothing -> span [ iconStyle
                        , class "glyphicon glyphicon-log-in"
                        , title "Login"
                        , onClick GoToLogin
                        ] []

view : Maybe Jwt -> Html Msg
view jwt = div [ style [ ("background-color", "#777")
                       , ("display", "flex")
                       , ("justify-content", "flex-end")
                       , ("padding", "16px 16px 0 16px") ] ]
                 [ loginOrNewPostIcon jwt
                 , span
                     [ iconStyle
                     , class "glyphicon glyphicon-home"
                     , title "All posts"
                     , onClick GoToPosts
                     ] []
                 ]
