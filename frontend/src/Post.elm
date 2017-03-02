module Post exposing (Model, Msg, init, view, update, mountCmd)

import ServerApi exposing (..)
import Routes
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { post : Maybe Post
    }


type Msg
    = HandlePostRetrieved (Result Http.Error Post)


init : Model
init =
    Model Nothing


mountCmd : Int -> Cmd Msg
mountCmd id =
    ServerApi.getPost id HandlePostRetrieved


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
                        (model, Cmd.none)





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
                            , ("background-color", "#333") ] ]
                    [ h2 [ style [ ("color", "#ddd") ] ]
                        [ text post.title ]
                    , div [ style [ ("color", "#fff") ] ] 
                        [ text post.content ]
                    ]
            Nothing ->
                h2 [ style [ ("color", "#fff")
                            , ("padding", "32px")
                            , ("width", "100vw")
                            , ("text-align", "center")
                            ] ]
                    [ text "No post found!" ]
        ]
