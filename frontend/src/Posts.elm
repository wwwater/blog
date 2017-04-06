module Posts exposing (Model, Msg, init, view, update, mountCmd)

import ServerApi exposing (..)
import Routes
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { posts : List Post
    , errors : List String
    }


type Msg
    = HandlePostsRetrieved (Result Http.Error (List Post))
    | GoToPost Int


init : Model
init =
    Model [] []


mountCmd : Cmd Msg
mountCmd =
    ServerApi.getPosts HandlePostsRetrieved


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of

        HandlePostsRetrieved res ->
            case res of
                Result.Ok posts ->
                    ( { model | posts = posts }
                    , Cmd.none
                    )

                Result.Err err ->
                    let _ = Debug.log "Error retrieving posts" err
                    in
                        (model, Cmd.none)

        GoToPost id -> (model, Routes.navigate (Routes.PostPage id))





------ VIEW ------


view : Model -> Html Msg
view model =
    div [ style [ ("background-color", "#777")
        , ("display", "flex")
        , ("justify-content", "center")
        , ("flex-grow", "1") ] ]
        [ div [ style [ ("width", "800px")
                      , ("margin", "0 32px") ] ]
            (List.map postPanel (List.reverse model.posts))
        ]

postTitle : Maybe String -> String
postTitle maybeTitle =
    let title = Maybe.withDefault "" maybeTitle
    in if title == "" then "<no title>" else title

postPanel : Post -> Html Msg
postPanel post =
    div
        [ style [ ("cursor", "pointer")
                , ("padding", "32px")
                , ("margin", "32px 0")
                , ("box-shadow", "2px 2px 15px 1px #333")
                , ("border-radius", "2px")
                , ("background-color", "#333") ]
        , onClick (GoToPost (Maybe.withDefault 0 post.id))
        ]
        [ h2 [ style [ ("color", "#ddd") ] ]
            [ text (postTitle post.title) ]
        , div [ style [ ("color", "#fff")
                      , ("white-space", "pre-wrap")
                      , ("text-align", "justify") ] ]
            [ let content = Maybe.withDefault "" post.content in
                if (String.length content > 1000)
                    then text (String.left 1000 content ++ "...")
                    else text content ]
        ]
