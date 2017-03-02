module Main exposing (..)

import Posts
import Post
import Routes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html
import Navigation


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { route : Routes.Route
    , postsModel : Posts.Model
    , postModel : Post.Model
    }


type Msg
    = PostsMsg Posts.Msg
    | PostMsg Post.Msg
    | Navigate String
    | UrlChange Navigation.Location


initialModel : Model
initialModel =
    { route = PostsPage
    , postsModel = Posts.init
    , postModel = Post.init
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    update (UrlChange loc) initialModel



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        PostsMsg m ->
            let
                ( subMdl, subCmd ) =
                    Posts.update m model.postsModel
            in
                { model | postsModel = subMdl }
                    ! [ Cmd.map PostsMsg subCmd ]
        
        PostMsg m ->
            let
                ( subMdl, subCmd ) =
                    Post.update m model.postModel
            in
                { model | postModel = subMdl }
                    ! [ Cmd.map PostMsg subCmd ]

        UrlChange loc ->
            urlUpdate loc model

        Navigate url ->
            model ! [ Navigation.newUrl url ]


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


view : Model -> Html Msg
view model = contentView model
        


contentView : Model -> Html Msg
contentView model =
    case model.route of
        PostsPage ->
            Html.map PostsMsg <| Posts.view model.postsModel
        
        PostPage id ->
            Html.map PostMsg <| Post.view model.postModel
