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
        , ("width", "100vw")
        , ("min-height", "100vh") ] ]
        [ div [ style [ ("width", "800px") ] ]
            (List.map postPanel model.posts)
        ]


postPanel : Post -> Html Msg
postPanel post =
    div [ style [ ("padding", "32px")
                , ("margin", "32px")
                , ("background-color", "#333") ] ]            
        [ Routes.linkTo (Routes.PostPage post.id) 
            [ style [ ("color", "transparent") ] ]
            [ h2 [ style [ ("color", "#ddd") ] ]
                [ text post.title ]
            ]
        , div [ style [ ("color", "#fff") ] ] 
            [ text post.content ]
        ]
