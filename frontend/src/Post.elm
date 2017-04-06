module Post             exposing ( Model
                                 , Msg
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 )

import Html             exposing (..)
import Html.Attributes  exposing (class, href, style, title)
import Html.Events      exposing (onClick)
import Http
import Global           exposing ( Msg (..)
                                 , handleServerErrorForPost
                                 , onlyUpdateModel
                                 )
import ServerApi        exposing (..)
import Routes



type alias Model =
    { post : Maybe Post
    , error : Maybe String
    }


type Msg
    = HandlePostRetrieved (Result Http.Error Post)
    | GoToEditPost Int
    | DeletePostOnServer Jwt
    | HandlePostDeleted (Result Http.Error String)


init : Model
init =
    Model Nothing Nothing


mountCmd : Int -> Cmd Msg
mountCmd id =
    ServerApi.getPost id HandlePostRetrieved


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








view : Model -> Maybe Jwt -> Html Msg
view model jwt =
    div [ style [ ("background-color", "#777")
                , ("display", "flex")
                , ("justify-content", "center")
                , ("flex-grow", "1")
                ] ]
        [ case model.post of
            Just post ->
                div [ style [ ("padding", "32px")
                            , ("margin", "32px")
                            , ("width", "800px")
                            , ("display", "flex")
                            , ("flex-direction", "column")
                            , ("background-color", "#333") ] ]
                    [ h2 [ style [ ("color", "#ddd") ] ]
                        [ text (Maybe.withDefault "" post.title) ]
                    , div [ style [ ("color", "#fff")
                                  , ("flex-grow", "1")
                                  , ("text-align", "justify")
                                  , ("white-space", "pre-wrap")
                                  ] ]
                        [ text (Maybe.withDefault "" post.content) ]
                    , div [ style [ ("font-size", "18px")
                                  , ("align-self", "flex-end")
                                  , ("margin-top", "16px")
                                  ] ]
                        [ case jwt of
                            Just jwt -> div []
                                [ a
                                    [ style [ ("color", "#fff")
                                            , ("margin-left", "16px")
                                            , ("cursor", "pointer")
                                            ]
                                    , class "glyphicon glyphicon-pencil"
                                    , title "Edit Post"
                                    , onClick (GoToEditPost (Maybe.withDefault 0 post.id))
                                    ] []
                                , span [ style [ ("color", "#fff")
                                               , ("margin-left", "16px")
                                               , ("cursor", "pointer") ]
                                       , class "glyphicon glyphicon-trash"
                                       , title "Delete post"
                                       , onClick (DeletePostOnServer jwt) ] []
                                ]
                            Nothing -> div [] []
                        ]
                    ]
            Nothing ->
                h2 [ style [ ("color", "#fff")
                            , ("padding", "32px")
                            , ("width", "100vw")
                            , ("text-align", "center")
                            ] ]
                    [ text (Maybe.withDefault "An unexpected error occured." model.error) ]
        ]
