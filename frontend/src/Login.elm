port module Login exposing (Model, Msg, init, view, update, mountCmd)

import ServerApi exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, value, placeholder, maxlength, type_)
import Html.Events exposing (onInput, onClick)
import Http
import Navigation exposing (back)

type alias Model =
    { credentials : Credentials
    , jwt : Maybe Jwt
    }


type Msg
    = HandleJwtReceived (Result Http.Error Jwt)
    | ChangeUsername String
    | ChangePassword String
    | SubmitCredentials


init : Model
init =
    Model {username = "", password = ""} Nothing


mountCmd : Cmd Msg
mountCmd = Cmd.none

port save : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleJwtReceived res ->
            case res of
                Result.Ok jwt ->
                    let _ = Debug.log "Received jwt" jwt in
                    ( { model |
                        jwt = Just jwt,
                        credentials = {username = "", password = ""} }
                    , Cmd.batch[ save jwt, Navigation.back 1]
                    )

                Result.Err err ->
                    let _ = Debug.log "Error getting jwt" err in
                    ( { model |
                        jwt = Nothing,
                        credentials = {username = "", password = ""} }
                    , Cmd.none
                    )
        ChangeUsername newUsername ->
            let creds = model.credentials in
            ( { model | credentials = { creds | username = newUsername } }
            , Cmd.none
            )

        ChangePassword newPassword ->
            let creds = model.credentials in
            ( { model | credentials = { creds | password = newPassword } }
            , Cmd.none
            )

        SubmitCredentials ->
            if model.credentials.username /= "" && model.credentials.password /= ""
            then let _ = Debug.log "Submitting credentials for" model.credentials.username
             in (model, ServerApi.getJwt model.credentials HandleJwtReceived)
            else (model, Cmd.none)



------ VIEW ------


view : Model -> Html Msg
view model =
    div [ style [ ("background-color", "#777")
                , ("display", "flex")
                , ("flex-direction", "column")
                , ("justify-content", "center")
                , ("align-items", "center")
                , ("min-height", "100vh") ] ]
        [ input [ style [ ("width", "400px")
                        , ("border-radius", "5px")
                        , ("padding", "8px")
                        , ("margin-bottom", "16px") ]
                , type_ "text"
                , value model.credentials.username
                , placeholder "Username"
                , maxlength 100
                , onInput ChangeUsername ] []
        , input [ style [ ("width", "400px")
                        , ("border-radius", "5px")
                        , ("padding", "8px")
                        , ("margin-bottom", "16px") ]
                , type_ "password"
                , value model.credentials.password
                , placeholder "Password"
                , maxlength 100
                , onInput ChangePassword ] []
        , button [ style [ ("width", "200px")
                         , ("font-weight", "bold")
                         , ("padding", "8px")
                         , ("border-radius", "5px") ]
                 , onClick SubmitCredentials ]
            [ text "Login" ]
        ]

