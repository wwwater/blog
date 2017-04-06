port module Main exposing (..)

import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html
import Navigation

import ServerApi        exposing (Jwt)
import Posts
import Post
import EditPost
import Login
import Menu
import Routes           exposing (..)
import GlobalMessages   exposing (Msg(..))

port save : String -> Cmd msg
port doload : () -> Cmd msg
port load : (String -> msg) -> Sub msg
port remove : () -> Cmd msg


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { route : Routes.Route
    , postsModel : Posts.Model
    , postModel : Post.Model
    , editPostModel: EditPost.Model
    , loginModel: Login.Model
    , jwt : Maybe Jwt
    }


type Msg
    = PostsMsg Posts.Msg
    | PostMsg Post.Msg
    | EditPostMsg EditPost.Msg
    | LoginMsg Login.Msg
    | MenuMsg Menu.Msg
    | Navigate String
    | UrlChange Navigation.Location
    | Load String
    | GlobalMsg GlobalMessages.Msg


initialModel : Model
initialModel =
    { route = PostsPage
    , postsModel = Posts.init
    , postModel = Post.init
    , editPostModel = EditPost.init
    , loginModel = Login.init
    , jwt = Nothing
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    let (mdl, cmd) = update (UrlChange loc) initialModel
    in (mdl, Cmd.batch[ cmd, doload () ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        PostsMsg m ->
            let ( subMdl, subCmd ) = Posts.update m model.postsModel
            in { model | postsModel = subMdl } ! [ Cmd.map PostsMsg subCmd ]

        PostMsg m ->
            let ( subMdl, subCmd ) = Post.update m model.postModel
            in { model | postModel = subMdl } ! [ Cmd.map PostMsg subCmd ]

        EditPostMsg m ->
            let
                ( subMdl, subCmd, globalMsg ) = EditPost.update m model.editPostModel
                ( mdl, cmd ) = update (GlobalMsg globalMsg) { model | editPostModel = subMdl }
            in mdl ! [ Cmd.map EditPostMsg subCmd, cmd ]

        LoginMsg m ->
            let
                ( subMdl, subCmd, globalMsg ) = Login.update m model.loginModel
                ( mdl, cmd ) = update (GlobalMsg globalMsg) { model | loginModel = subMdl }
            in mdl ! [ Cmd.map LoginMsg subCmd, cmd ]

        MenuMsg m ->
            let ( subMdl, subCmd ) = Menu.update m {}
            in model ! [ Cmd.map MenuMsg subCmd ]

        UrlChange loc ->
            urlUpdate loc model

        Navigate url ->
            model ! [ Navigation.newUrl url ]

        Load jwt ->
            let _ = Debug.log "Loaded jwt from local storage" jwt in
            { model | jwt = Just jwt } ! []

        GlobalMsg m ->
            case m of
                None -> model ! []
                SaveJwt jwt -> { model | jwt = Just jwt } ! [ save jwt ]
                RemoveJwt -> { model | jwt = Nothing } ! [ remove () ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map EditPostMsg (EditPost.subscriptions model.editPostModel
            (Maybe.withDefault "" model.jwt))
        , load Load
        ]


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate loc model =
    case (Routes.decode loc) of
        Nothing  ->
            model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just (PostsPage as route) ->
            { model | route = route }
                ! [ Cmd.map PostsMsg Posts.mountCmd ]

        Just ((PostPage postId) as route) ->
            { model | route = route }
                ! [ Cmd.map PostMsg <| Post.mountCmd postId ]

        Just ((EditPostPage postId) as route) ->
            case model.jwt of
                Just jwt ->
                    { model | route = route }
                        ! [ Cmd.map EditPostMsg <| EditPost.mountCmd (Just postId) ]
                Nothing ->
                    model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just (NewPostPage as route) ->
            case model.jwt of
                Just jwt ->
                    { model | route = route, editPostModel = EditPost.init }
                        ! [ Cmd.map EditPostMsg <| EditPost.mountCmd Nothing ]
                Nothing ->
                    model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just (LoginPage as route) ->
            { model | route = route }
                ! [ Cmd.map LoginMsg <| Login.mountCmd ]



view : Model -> Html Msg
view model = div []
    [ Html.map MenuMsg <| Menu.view model.jwt
    , contentView model
    ]



contentView : Model -> Html Msg
contentView model =
    case model.route of
        PostsPage ->
            Html.map PostsMsg <| Posts.view model.postsModel

        PostPage id ->
            Html.map PostMsg <| Post.view model.postModel model.jwt

        EditPostPage id ->
            Html.map EditPostMsg <| EditPost.view model.editPostModel
             (Maybe.withDefault "" model.jwt)

        NewPostPage ->
            Html.map EditPostMsg <| EditPost.view model.editPostModel
             (Maybe.withDefault "" model.jwt)

        LoginPage ->
            Html.map LoginMsg <| Login.view model.loginModel
