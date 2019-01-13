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
import Global           exposing (Msg(..))

port save : String -> Cmd msg
port remove : () -> Cmd msg


type alias Flags = { jwt : String }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
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
    | GlobalMsg Global.Msg


initialModel : Flags -> Model
initialModel flags =
    { route = PostsPage
    , postsModel = Posts.init
    , postModel = Post.init
    , editPostModel = EditPost.init
    , loginModel = Login.init
    , jwt = if flags.jwt /= "" then Just flags.jwt else Nothing
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags loc = update (UrlChange loc) <| initialModel flags


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        PostsMsg m ->
            let ( subMdl, subCmd, globalMsg ) = Posts.update m model.postsModel
                ( mdl, cmd ) = update (GlobalMsg globalMsg) { model | postsModel = subMdl }
            in mdl ! [ Cmd.map PostsMsg subCmd, cmd ]

        PostMsg m ->
            let ( subMdl, subCmd, globalMsg ) = Post.update m model.postModel
                ( mdl, cmd ) = update (GlobalMsg globalMsg) { model | postModel = subMdl }
            in mdl ! [ Cmd.map PostMsg subCmd, cmd ]

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

        GlobalMsg m ->
            case m of
                None -> model ! []
                SaveJwt jwt -> { model | jwt = Just jwt } ! [ save jwt ]
                RemoveJwt -> { model | jwt = Nothing } ! [ remove () ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map EditPostMsg (EditPost.subscriptions model.editPostModel
        (Maybe.withDefault "" model.jwt))


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate loc model =
    case (Routes.decode loc) of
        Nothing  ->
            model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just (PostsPage as route) ->
            { model | route = route, postsModel = Posts.init }
                ! [ Cmd.map PostsMsg <| Posts.mountCmd model.jwt]

        Just ((PostPage postId) as route) ->
            { model | route = route, postModel = Post.init }
                ! [ Cmd.map PostMsg <| Post.mountCmd postId model.jwt]

        Just ((EditPostPage postId) as route) ->
            case model.jwt of
                Just jwt ->
                    { model | route = route }
                        ! [ Cmd.map EditPostMsg <|
                            EditPost.mountCmd (Just postId) model.jwt ]
                Nothing ->
                    model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just (NewPostPage as route) ->
            case model.jwt of
                Just jwt ->
                    let
                        editPostModel = model.editPostModel
                        mdl =
                            if editPostModel.postOnClient.id /= Nothing
                                -- init when model contained a post under edit
                                -- TODO maybe fix this by separating new and edit
                                -- components somehow
                                then { model |
                                    route = route,
                                    editPostModel = EditPost.init }
                                else { model |
                                    route = route,
                                    editPostModel = { editPostModel | error = Nothing } }
                    in mdl ! [ Cmd.map EditPostMsg <| EditPost.mountCmd Nothing Nothing ]
                Nothing ->
                    model ! [ Navigation.modifyUrl (Routes.encode model.route) ]

        Just (LoginPage as route) ->
            { model | route = route }
                ! [ Cmd.map LoginMsg <| Login.mountCmd ]



view : Model -> Html Msg
view model = div [ style [ ("display", "flex")
                         , ("flex-direction", "column")
                         , ("min-height", "100vh")
                         , ("font-family", "Ubuntu,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Geneva,Verdana,sans-serif")
                         ] ]
    [ Html.map MenuMsg <| Menu.view model.jwt
    , contentView model
    ]



contentView : Model -> Html Msg
contentView model =
    case model.route of
        PostsPage ->
            Html.map PostsMsg <| Posts.view model.postsModel model.jwt

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
