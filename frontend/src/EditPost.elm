module EditPost         exposing ( Model
                                 , Msg (..)
                                 , init
                                 , view
                                 , update
                                 , mountCmd
                                 , subscriptions )

import Html             exposing (..)
import Html.Attributes  exposing ( class
                                 , href
                                 , style
                                 , value
                                 , placeholder
                                 , maxlength
                                 , title )
import Html.Events      exposing ( onClick, onInput )
import Http
import Time             exposing ( Time, minute )

import ServerApi        exposing ( Post, Jwt )
import Global           exposing ( Msg(..)
                                 , handleServerErrorForPost
                                 , onlyUpdateModel )
import Routes



type alias Model =
    { postOnServer : Maybe Post
    , postOnClient : Maybe Post
    , error : Maybe String
    }


type Msg
    = HandlePostRetrieved (Result Http.Error Post)
    | ChangePostContentOnClient String
    | ChangePostTitleOnClient String
    | UpdatePostOnServer Jwt
    | Tick Jwt Time


emptyPost : Post
emptyPost = { id = Nothing, title = Just "", content = Just "" }

init : Model
init =
    Model
        Nothing
        (Just emptyPost)
        Nothing


mountCmd : Maybe Int -> Cmd Msg
mountCmd id =
    case id of
        Just postId ->
            ServerApi.getPost postId HandlePostRetrieved
        Nothing -> Cmd.none


requiredPostSaving : Model -> Bool
requiredPostSaving model =
    case model.postOnClient of
        Just post ->
            case model.postOnServer of
                Just postOnServer -> post /= postOnServer
                Nothing -> post /= emptyPost
        Nothing -> False


update : Msg -> Model -> ( Model, Cmd Msg, Global.Msg )
update action model =
    case action of
        HandlePostRetrieved res ->
            case res of
                Result.Ok post ->
                    let _ = Debug.log "Received updated post" post.id in
                    onlyUpdateModel { model |
                          postOnServer = Just post
                        , postOnClient = Just post
                        , error = Nothing }

                Result.Err err -> handleServerErrorForPost
                    { model | postOnServer = Nothing , postOnClient = Nothing }
                    err

        ChangePostTitleOnClient newTitle ->
            case model.postOnClient of
                Just justPost ->
                    onlyUpdateModel { model | postOnClient = Just { justPost | title = Just newTitle } }
                Nothing -> onlyUpdateModel model

        ChangePostContentOnClient newContent ->
            case model.postOnClient of
                Just justPost ->
                    onlyUpdateModel { model | postOnClient = Just { justPost | content = Just newContent } }
                Nothing -> onlyUpdateModel model

        UpdatePostOnServer jwt ->
            case model.postOnClient of
                Just post ->
                    let _ = Debug.log "Updating post on server" post.id
                    in (model, ServerApi.updatePost post jwt HandlePostRetrieved, Global.None)
                Nothing -> onlyUpdateModel model

        Tick jwt _ ->
            case model.postOnClient of
                Just post ->
                    if requiredPostSaving model
                        then let _ = Debug.log "Updating post on server by timer" post.id
                            in (model, ServerApi.updatePost post jwt HandlePostRetrieved, Global.None)
                        else onlyUpdateModel model
                Nothing -> onlyUpdateModel model





subscriptions : Model -> Jwt -> Sub Msg
subscriptions model jwt =
  Time.every minute (Tick jwt)





renderUpdateButton : Model -> Jwt -> Html Msg
renderUpdateButton model jwt =
    if model.postOnClient /= model.postOnServer
    then span [ style [ ("color", "#fff")
                      , ("cursor", "pointer") ]
              , class "glyphicon glyphicon-floppy-disk"
              , title "Save changes"
              , onClick (UpdatePostOnServer jwt) ] []
    else div [] []


view : Model -> Jwt -> Html Msg
view model jwt =
    div [ style [ ("background-color", "#777")
                , ("display", "flex")
                , ("justify-content", "center")
                , ("min-height", "100vh") ] ]
        [ case model.postOnClient of
            Just post ->
                div [ style [ ("padding", "32px")
                            , ("margin", "32px")
                            , ("width", "800px")
                            , ("display", "flex")
                            , ("flex-direction", "column")
                            , ("background-color", "#333") ] ]
                    [ textarea [ style [ ("color", "#ddd")
                                       , ("width", "100%")
                                       , ("background-color", "inherit")
                                       , ("border", "none")
                                       , ("font-size", "30px")
                                       , ("padding", "0 8px")
                                       , ("resize", "vertical")
                                       , ("margin", "16px 0") ]
                               , class "test-edit-post-title"
                               , maxlength 200
                               , value (Maybe.withDefault "" post.title)
                               , placeholder "..type here the title of the post"
                               , onInput ChangePostTitleOnClient ] []
                    , textarea [ style [ ("color", "#fff")
                                       , ("background-color", "inherit")
                                       , ("flex-grow", "1")
                                       , ("margin-bottom", "32px")
                                       , ("padding", "8px")
                                       , ("resize", "none")
                                       , ("border", "none") ]
                               , class "test-edit-post-content"
                               , value (Maybe.withDefault "" post.content)
                               , maxlength 10000
                               , placeholder "..type here the content of the post"
                               , onInput ChangePostContentOnClient ] []
                    , div [ style [ ("display", "flex")
                                  , ("align-items", "baseline")
                                  , ("align-self", "flex-end")
                                  , ("font-size", "18px")
                                  ] ]
                        [ renderUpdateButton model jwt ]
                    ]
            Nothing ->
                h2 [ style [ ("color", "#fff")
                            , ("padding", "32px")
                            , ("width", "100vw")
                            , ("text-align", "center")
                            ] ]
                    [ text (Maybe.withDefault "An unexpected error occured." model.error) ]
        ]
