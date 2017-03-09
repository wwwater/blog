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





------ VIEW ------


view : Model -> Html Msg
view model =
    div [ style [ ("background-color", "#777")
        , ("display", "flex")
        , ("flex-direction", "col")
        , ("justify-content", "center")
        , ("min-height", "100vh") ] ]
        [ div [ style [ ("width", "800px") ] ]
            (List.map postPanel model.posts)
        ]


postPanel : Post -> Html Msg
postPanel post =
    Routes.linkTo (Routes.PostPage (Maybe.withDefault 0 post.id))
        [ style [ ("display", "block")
                , ("padding", "32px")
                , ("color", "transparent")
                , ("margin", "32px")
                , ("background-color", "#333") ]
        ]
        [ h2 [ style [ ("color", "#ddd") ] ]
            [ text (Maybe.withDefault "<no title>" post.title) ]
        , div [ style [ ("color", "#fff")
                      , ("text-align", "justify") ] ]
            [ let content = Maybe.withDefault "" post.content in
                if (String.length content > 1000)
                    then text (String.left 1000 content ++ "...")
                    else text content ]
        ]
