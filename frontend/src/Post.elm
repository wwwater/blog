module Post             exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing ( class, href, style, title )
import Html.Events      exposing ( onClick )
import Http

import Global           exposing ( Msg (..)
                                 , handleServerErrorForPost
                                 , onlyUpdateModel
                                 )
import Util             exposing ( formatCreationDate
                                 , renderPostContent
                                 )
import ServerApi        exposing (..)
import Routes
import Styles           exposing (..)



type alias Model =
    { post : Maybe Post
    , error : Maybe String
    }


type Msg
    = HandlePostRetrieved (Result Http.Error Post)
    | GoToEditPost Int
    | PublishPost Jwt
    | DeletePostOnServer Jwt
    | HandlePostDeleted (Result Http.Error String)


init : Model
init =
    Model Nothing Nothing


mountCmd : Int -> Maybe Jwt -> Cmd Msg
mountCmd id maybeJwt =
    ServerApi.getPost id maybeJwt HandlePostRetrieved


update : Msg -> Model -> ( Model, Cmd Msg, Global.Msg )
update action model =
    case action of
        HandlePostRetrieved res ->
            case res of
                Result.Ok post ->
                    onlyUpdateModel { model | post = Just post }

                Result.Err err ->
                    handleServerErrorForPost { model | post = Nothing } err

        HandlePostDeleted res ->
            case res of
                Result.Ok _ ->
                    let _ = Debug.log "Post deleted" model.post in
                    ( model
                    , Routes.navigate Routes.PostsPage
                    , Global.None
                    )
                Result.Err err ->
                    handleServerErrorForPost model err

        GoToEditPost id ->
            ( model
            , Routes.navigate (Routes.EditPostPage id)
            , Global.None
            )

        DeletePostOnServer jwt ->
            case model.post of
                Just post ->
                    case post.id of
                        Just id ->
                            ( model
                            , ServerApi.deletePost id jwt HandlePostDeleted
                            , Global.None
                            )
                        Nothing -> onlyUpdateModel model
                Nothing -> onlyUpdateModel model

        PublishPost jwt ->
            case model.post of
                Just post ->
                    case post.id of
                        Just id ->
                            ( model
                            , ServerApi.publishPost id jwt HandlePostRetrieved
                            , Global.None
                            )
                        Nothing -> onlyUpdateModel model
                Nothing -> onlyUpdateModel model



renderPublishIcon : Jwt -> Maybe Bool -> Html Msg
renderPublishIcon jwt maybePublished =
    let published = Maybe.withDefault False maybePublished in
        if published
            then span [] []
            else
                span
                    [ iconStyle
                    , class "glyphicon glyphicon-globe"
                    , title "Publish post"
                    , onClick (PublishPost jwt)
                    ] []



view : Model -> Maybe Jwt -> Html Msg
view model jwt =
    div [ style [ ("background-color", "#777")
                , ("display", "flex")
                , ("justify-content", "center")
                , ("flex-grow", "1")
                ] ]
        [ case model.error of
            Just error ->
                h2 [ errorStyle ] [ text error ]
            Nothing ->
                case model.post of
                    Just post ->
                        div [ postStyle ]
                            [ div [ style [ ("align-self", "flex-end") ] ]
                                [ text <| formatCreationDate post.createdAt ]
                            , h2 [ style [ ("margin-bottom", "32px") ] ]
                                [ text (Maybe.withDefault "" post.title) ]
                            , div [ style [ ("flex-grow", "1")
                                          , ("text-align", "justify")
                                          , ("white-space", "pre-wrap")
                                          ] ]
                                (renderPostContent (Maybe.withDefault "" post.content))
                            , div [ style [ ("font-size", "18px")
                                          , ("align-self", "flex-end")
                                          , ("margin-top", "16px")
                                          ] ]
                                [ case jwt of
                                    Just jwt -> div []
                                        [ span
                                            [ iconStyle
                                            , class "glyphicon glyphicon-pencil"
                                            , title "Edit Post"
                                            , onClick (GoToEditPost (Maybe.withDefault 0 post.id))
                                            ] []
                                        , renderPublishIcon jwt post.published
                                        , span
                                            [ iconStyle
                                            , class "glyphicon glyphicon-trash"
                                            , title "Delete post"
                                            , onClick (DeletePostOnServer jwt)
                                            ] []
                                        ]
                                    Nothing -> div [] []
                                ]
                            ]
                    Nothing -> div [] []
        ]
