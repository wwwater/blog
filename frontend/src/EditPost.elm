module EditPost exposing (Model, Msg, init, view, update, mountCmd,
 subscriptions)

import ServerApi exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, href, style, value, placeholder,
 maxlength, title)
import Html.Events exposing (onClick, onInput)
import Http
import Time exposing (Time, minute)


type alias Model =
    { postOnServer : Maybe Post
    , postOnClient : Maybe Post
    }


type Msg
    = HandlePostRetrieved (Result Http.Error Post)
    | ChangePostContentOnClient String
    | ChangePostTitleOnClient String
    | UpdatePostOnServer Jwt
    | Tick Jwt Time


init : Model
init =
    Model Nothing (Just { id = Nothing, title = Just "", content = Just "" })


mountCmd : Maybe Int -> Cmd Msg
mountCmd id =
    case id of
        Just postId ->
            ServerApi.getPost postId HandlePostRetrieved
        Nothing -> Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandlePostRetrieved res ->
            case res of
                Result.Ok post ->
                    let _ = Debug.log "Received updated post" post.id in
                    ( { model |
                        postOnServer = Just post,
                        postOnClient = Just post }
                    , Cmd.none
                    )

                Result.Err err ->
                    let _ = Debug.log "Error retrieving post" err in
                    ( { model |
                        postOnServer = Nothing,
                        postOnClient = Nothing }
                    , Cmd.none
                    )
        ChangePostTitleOnClient newTitle ->
            case model.postOnClient of
                Just justPost ->
                    ( { model | postOnClient = Just { justPost | title = Just newTitle } }
                    , Cmd.none
                    )
                Nothing -> (model, Cmd.none)

        ChangePostContentOnClient newContent ->
            case model.postOnClient of
                Just justPost ->
                    ( { model | postOnClient = Just { justPost | content = Just newContent } }
                    , Cmd.none
                    )
                Nothing -> (model, Cmd.none)

        UpdatePostOnServer jwt ->
            case model.postOnClient of
                Just post ->
                    let _ = Debug.log "Updating post on server" post.id
                    in (model, ServerApi.updatePost post jwt HandlePostRetrieved)
                Nothing -> (model, Cmd.none)
        Tick jwt _ ->
            case model.postOnClient of
                Just post ->
                    if model.postOnClient /= model.postOnServer
                    then let _ = Debug.log "Updating post on server by timer" post.id
                        in (model, ServerApi.updatePost post jwt HandlePostRetrieved)
                    else (model, Cmd.none)
                Nothing -> (model, Cmd.none)




-- SUBSCRIPTIONS --

subscriptions : Model -> Jwt -> Sub Msg
subscriptions model jwt =
  Time.every minute (Tick jwt)




------ VIEW ------

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
                    [ text "No post found!" ]
        ]
