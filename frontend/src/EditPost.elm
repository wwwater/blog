module EditPost exposing (Model, Msg, init, view, update, mountCmd)

import ServerApi exposing (..)
import Routes
import Html exposing (..)
import Html.Attributes exposing (class, href, style, value, placeholder)
import Html.Events exposing (onClick, onInput)
import Http

type alias Model =
    { post : Maybe Post
    }


type Msg
    = HandlePostRetrieved (Result Http.Error Post)
    | ChangePost String
    | UpdatePost


init : Model
init =
    Model (Just { id = Nothing, title = Just "", content = Just "" })


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
                    ( { model | post = Just post }
                    , Cmd.none
                    )

                Result.Err err ->
                    let _ = Debug.log "Error retrieving post" err
                    in
                        ({ model | post = Nothing }, Cmd.none)
        ChangePost newContent ->
            case model.post of
                Just justPost ->
                    ( { model | post = Just { justPost | content = Just newContent } }
                    , Cmd.none
                    )
                Nothing -> (model, Cmd.none)
        UpdatePost ->
            case model.post of
                Just post ->
                    let _ = Debug.log "Clicked"
                    in (model, ServerApi.updatePost post HandlePostRetrieved)
                Nothing -> (model, Cmd.none)





------ VIEW ------

view : Model -> Html Msg
view model =
    div [ style [ ("background-color", "#777")
                , ("display", "flex")
                , ("justify-content", "center")
                , ("width", "100vw")
                , ("min-height", "100vh") ] ] 
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
                    , textarea [ style [ ("color", "#fff")
                                    , ("background-color", "inherit")
                                    , ("flex-grow", "1")
                                    , ("margin-bottom", "32px")
                                    , ("resize", "none")
                                    , ("border", "none") ]
                               , value (Maybe.withDefault "" post.content)
                               , placeholder "..type here the content of the post"
                               , onInput ChangePost ] []
                    , button [ style [ ("background-color", "#3c6d3d")
                                     , ("color", "#fff")
                                     , ("border-radius", "3px")
                                     , ("border-color", "#5eab60")
                                     , ("padding", "6px")
                                     , ("font-size", "18px") ]
                             , onClick UpdatePost ] 
                        [ case post.id of
                            Just postId -> text "Update"
                            Nothing -> text "Create"
                        ]
                    ]
            Nothing ->
                h2 [ style [ ("color", "#fff")
                            , ("padding", "32px")
                            , ("width", "100vw")
                            , ("text-align", "center")
                            ] ]
                    [ text "No post found!" ]
        ]
