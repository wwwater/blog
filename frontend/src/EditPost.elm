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
import Styles           exposing (..)


type alias Model =
    { postOnServer : Maybe Post
    , postOnClient : Post
    , error : Maybe String
    }


type Msg
    = HandlePostRetrieved (Result Http.Error Post)
    | ChangePostContentOnClient String
    | ChangePostTitleOnClient String
    | UpdatePostOnServer Jwt
    | Tick Jwt Time


emptyPost : Post
emptyPost = { id = Nothing
            , title = Nothing
            , content = Nothing
            , createdAt = Nothing
            , published = Nothing
            }

init : Model
init =
    Model
        Nothing
        emptyPost
        Nothing


mountCmd : Maybe Int -> Maybe Jwt -> Cmd Msg
mountCmd id maybeJwt =
    case id of
        Just postId ->
            ServerApi.getPost postId maybeJwt HandlePostRetrieved
        Nothing -> Cmd.none


requiredPostSaving : Model -> Bool
requiredPostSaving model =
    case model.postOnServer of
        Just postOnServer -> model.postOnClient /= postOnServer
        Nothing -> model.postOnClient /= emptyPost

requiredPostOnClientRewriting : Model -> Post -> Bool
requiredPostOnClientRewriting model newPost =
    case model.postOnServer of
        -- if this post was already loaded once, do not update
        -- postOnClient with the post from server
        Just postOnServer -> postOnServer.id /= newPost.id
        Nothing -> True

update : Msg -> Model -> ( Model, Cmd Msg, Global.Msg )
update action model =
    case action of
        HandlePostRetrieved res ->
            case res of
                Result.Ok post ->
                    let _ = Debug.log "Received updated post with id" post.id in
                    if requiredPostOnClientRewriting model post
                        then onlyUpdateModel { model |
                              postOnServer = Just post
                            , postOnClient = post
                            , error = Nothing }
                        else onlyUpdateModel { model |
                              postOnServer = Just post
                            , error = Nothing }

                Result.Err err -> handleServerErrorForPost model err

        ChangePostTitleOnClient newTitle ->
            let post = model.postOnClient in
            onlyUpdateModel { model | postOnClient = { post | title = Just newTitle } }

        ChangePostContentOnClient newContent ->
            let post = model.postOnClient in
            onlyUpdateModel { model | postOnClient = { post | content = Just newContent } }

        UpdatePostOnServer jwt ->
            let _ = Debug.log "Updating post on server with id" model.postOnClient.id
            in (model, ServerApi.updatePost model.postOnClient jwt HandlePostRetrieved, Global.None)

        Tick jwt _ ->
            if requiredPostSaving model
                then
                    let _ = Debug.log "Updating post on server by timer with id" model.postOnClient.id
                    in (model, ServerApi.updatePost model.postOnClient jwt HandlePostRetrieved, Global.None)
                else onlyUpdateModel model





subscriptions : Model -> Jwt -> Sub Msg
subscriptions model jwt =
  Time.every minute (Tick jwt)





renderUpdateButton : Model -> Jwt -> Html Msg
renderUpdateButton model jwt =
    if (Just model.postOnClient) /= model.postOnServer
    then span [ iconStyle
              , class "glyphicon glyphicon-floppy-disk"
              , title "Save changes"
              , onClick (UpdatePostOnServer jwt) ] []
    else div [] []


view : Model -> Jwt -> Html Msg
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
                div [ postStyle ]
                    [ textarea [ style [ ("width", "100%")
                                       , ("background-color", "inherit")
                                       , ("border", "none")
                                       , ("font-size", "30px")
                                       , ("padding", "0 8px")
                                       , ("resize", "vertical")
                                       , ("margin", "16px 0") ]
                               , class "test-edit-post-title"
                               , maxlength 200
                               , value (Maybe.withDefault "" model.postOnClient.title)
                               , placeholder "..type here the title of the post"
                               , onInput ChangePostTitleOnClient ] []
                    , textarea [ style [ ("background-color", "inherit")
                                       , ("flex-grow", "1")
                                       , ("margin-bottom", "16px")
                                       , ("padding", "8px")
                                       , ("resize", "none")
                                       , ("border", "none") ]
                               , class "test-edit-post-content"
                               , value (Maybe.withDefault "" model.postOnClient.content)
                               , maxlength 10000
                               , placeholder "..type here the content of the post"
                               , onInput ChangePostContentOnClient ] []
                    , div [ style [ ("align-self", "flex-end")
                                  , ("margin-bottom", "-16px")
                                  , ("height", "34px") ] ]
                        [ renderUpdateButton model jwt ]
                    ]
        ]
