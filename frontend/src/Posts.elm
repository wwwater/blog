module Posts                exposing ( Model
                                     , Msg
                                     , init
                                     , view
                                     , update
                                     , mountCmd )

import Html                 exposing (..)
import Html.Attributes      exposing ( class
                                     , href
                                     , style
                                     )
import Html.Events          exposing ( onClick )
import Http

import ServerApi            exposing (..)
import Routes
import Styles               exposing (..)
import Util                 exposing ( formatCreationDate
                                     , renderPostContent
                                     )


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


mountCmd : Maybe Jwt -> Cmd Msg
mountCmd maybeJwt =
    ServerApi.getPosts maybeJwt HandlePostsRetrieved


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





view : Model -> Html Msg
view model =
    div [ style [ ("background-color", "#777")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("align-items", "center")
        , ("flex-grow", "1") ] ]
        (List.map postPanel model.posts)


postTitle : Maybe String -> String
postTitle maybeTitle =
    let title = Maybe.withDefault "" maybeTitle
    in if title == "" then "<no title>" else title

postPanel : Post -> Html Msg
postPanel post =
    div
        [ style [ ("cursor", "pointer") ]
        , postStyle
        , onClick (GoToPost (Maybe.withDefault 0 post.id))
        ]
        [ div [ style [ ("align-self", "flex-end") ] ]
            [ text <| formatCreationDate post.createdAt ]
        , h2 [ style [ ("margin-top", "8px")
                     , ("margin-bottom", "32px") ] ]
            [ text (postTitle post.title) ]
        , div [ style [ ("white-space", "pre-wrap")
                      , ("text-align", "justify") ] ]
            (renderPostContent (Maybe.withDefault "" post.content))
        ]
