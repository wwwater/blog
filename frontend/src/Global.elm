module Global exposing ( Msg(..)
                       , handleServerErrorForPost
                       , onlyUpdateModel
                       , formatCreationDate
                       )

import Http
import Date      exposing ( fromTime, day, month, year )

import ServerApi exposing (Jwt)


type Msg
    = None
    | SaveJwt Jwt
    | RemoveJwt


getError : Http.Error -> (String, Maybe Int)
getError err =
    case err of
        Http.BadStatus badStatus ->
            (case badStatus.status.code of
                404 -> "Post not found!"
                401 -> badStatus.body
                _ -> badStatus.status.message
            , Just badStatus.status.code)
        Http.BadUrl text ->
            ("Bad url " ++ text, Nothing)
        Http.Timeout ->
            ("Timeout", Nothing)
        Http.BadPayload message _ ->
            ("Bad payload " ++ message, Nothing)
        Http.NetworkError ->
            ("Network error", Nothing)

handleServerErrorForPost : { m | error: Maybe String } -> Http.Error ->
    ( { m | error : Maybe String }, Cmd b, Msg )
handleServerErrorForPost model err =
    let (errorAsString, code) = getError err
        _ = Debug.log "An error occured in request" errorAsString in
    ( { model | error = Just errorAsString }
    , Cmd.none
    , case code of
        Just code ->
            case code of
                401 -> RemoveJwt
                _ -> None
        Nothing -> None
    )

onlyUpdateModel : m -> ( m, Cmd msg, Msg )
onlyUpdateModel model = ( model, Cmd.none, None )

formatCreationDate : Maybe Int -> String
formatCreationDate createdAt =
    case createdAt of
        Just unixTime ->
            let date = fromTime (toFloat unixTime * 1000)
                d = toString <| day date
                m = toString <| month date
                y = toString <| year date
            in d ++ " " ++ m ++ " " ++ y
        Nothing -> "Unknown posting date"
