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
                                     , title
                                     )
import Html.Events          exposing ( onClick )
import Http

import ServerApi            exposing (..)
import Routes
import Styles               exposing (..)
import Util                 exposing ( formatCreationDate
                                     , renderPostContent
                                     )
import Global               exposing ( Msg(..)
                                     , handleServerErrorForPost
                                     , onlyUpdateModel )


type alias Model =
    { posts : List Post
    , error : Maybe String
    , countLoadedPosts : Int
    , showMorePostsButton : Bool
    }


type Msg
    = HandlePostsRetrieved (Result Http.Error (List Post))
    | GoToPost Int
    | RetrieveMorePosts (Maybe Jwt)


init : Model
init =
    Model [] Nothing 0 True


mountCmd : Maybe Jwt -> Cmd Msg
mountCmd maybeJwt =
    ServerApi.getPosts Nothing maybeJwt HandlePostsRetrieved


update : Msg -> Model -> ( Model, Cmd Msg, Global.Msg )
update action model =
    case action of

        HandlePostsRetrieved res ->
            case res of
                Result.Ok posts ->
                    let countNewPosts = List.length posts in
                    onlyUpdateModel { model |
                        posts = model.posts ++ posts,
                        error = Nothing,
                        countLoadedPosts = model.countLoadedPosts + countNewPosts,
                        showMorePostsButton = countNewPosts > 0}

                Result.Err err -> handleServerErrorForPost model err

        GoToPost id -> (model, Routes.navigate (Routes.PostPage id), Global.None)

        RetrieveMorePosts maybeJwt ->
            ( model
            , ServerApi.getPosts (Just model.countLoadedPosts) maybeJwt HandlePostsRetrieved
            , Global.None
            )




view : Model -> Maybe Jwt -> Html Msg
view model maybeJwt =
    div [ style [ ("background-color", "#777")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("align-items", "center")
        , ("flex-grow", "1") ] ]
        <|
        case model.error of
          Just error ->
              [ h2 [ errorStyle ] [ text error ] ]
          Nothing ->
              (List.map postPanel model.posts) ++
              [morePostsButton model.showMorePostsButton maybeJwt]


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

morePostsButton : Bool -> Maybe Jwt -> Html Msg
morePostsButton showButton maybeJwt =
    if showButton
    then
        span
            [ iconStyle
            , style [ ("margin-bottom", "16px") ]
            , class "glyphicon glyphicon-option-horizontal"
            , title "...get more posts"
            , onClick (RetrieveMorePosts maybeJwt)
            ] []
    else
        span [] []
