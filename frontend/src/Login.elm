module Login exposing (Model, Msg, init, view, update, mountCmd)

import Html             exposing (..)
import Html.Attributes  exposing ( style, value, placeholder, maxlength, type_ )
import Html.Events      exposing ( onInput, onClick )
import Http
import Navigation       exposing ( back )

import ServerApi        exposing (..)
import Global           exposing ( Msg(..) )
import Styles           exposing (..)

type alias Model =
    { credentials : Credentials }


type Msg
    = HandleJwtReceived (Result Http.Error Jwt)
    | ChangeUsername String
    | ChangePassword String
    | SubmitCredentials


init : Model
init =
    Model {username = "", password = ""}


mountCmd : Cmd Msg
mountCmd = Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg, Global.Msg )
update action model =
    case action of
        HandleJwtReceived res ->
            case res of
                Result.Ok jwt ->
                    let _ = Debug.log "Received jwt" jwt in
                    ( { model | credentials = {username = "", password = ""} }
                    , Navigation.back 1
                    , Global.SaveJwt jwt
                    )

                Result.Err err ->
                    let _ = Debug.log "Error getting jwt" err in
                    ( { model | credentials = {username = "", password = ""} }
                    , Cmd.none
                    , Global.None
                    )
        ChangeUsername newUsername ->
            let creds = model.credentials in
            ( { model | credentials = { creds | username = newUsername } }
            , Cmd.none
            , Global.None
            )

        ChangePassword newPassword ->
            let creds = model.credentials in
            ( { model | credentials = { creds | password = newPassword } }
            , Cmd.none
            , Global.None
            )

        SubmitCredentials ->
            if model.credentials.username /= "" && model.credentials.password /= ""
                then
                    let _ = Debug.log "Submitting credentials for" model.credentials.username
                    in (
                        model
                        , ServerApi.getJwt model.credentials HandleJwtReceived
                        , Global.None
                        )
            else (
                model
                , Cmd.none
                , Global.None
                )




view : Model -> Html Msg
view model =
    div [ style [ ("background-color", "#777")
                , ("display", "flex")
                , ("flex-direction", "column")
                , ("justify-content", "center")
                , ("align-items", "center")
                , ("flex-grow", "1") ] ]
        [ input [ style [ ("width", "320px")
                        , ("background-color", "#eee")
                        , ("color", "#333")
                        , ("margin-bottom", "16px") ]
                , formStyle
                , type_ "text"
                , value model.credentials.username
                , placeholder "Username"
                , maxlength 100
                , onInput ChangeUsername ] []
        , input [ style [ ("width", "320px")
                        , ("background-color", "#eee")
                        , ("color", "#333")
                        , ("margin-bottom", "16px") ]
                , formStyle
                , type_ "password"
                , value model.credentials.password
                , placeholder "Password"
                , maxlength 100
                , onInput ChangePassword ] []
        , button [ style [ ("width", "160px")
                         , ("background-color", "#555")
                         , ("color", "#eee")
                         , ("font-weight", "bold") ]
                 , formStyle
                 , onClick SubmitCredentials ]
            [ text "Login" ]
        ]

