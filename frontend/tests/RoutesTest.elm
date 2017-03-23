module RoutesTest exposing (..)

import Test         exposing (..)
import Expect

import Navigation   exposing (Location)
import Routes       exposing (encode)


all : Test
all =
    describe "testing Routes"
        [ describe "encoding routes"
            [ test "/posts" <|
                \() -> Routes.encode Routes.PostsPage
                |> Expect.equal "/"
            , test "/post/{id}" <|
                \() -> Routes.encode (Routes.PostPage 5)
                |> Expect.equal "/post/5"
            , test "/edit/{id}" <|
                \() -> Routes.encode (Routes.EditPostPage 7)
                |> Expect.equal "/edit/7"
            , test "/new" <|
                \() -> Routes.encode Routes.NewPostPage
                |> Expect.equal "/new"
            , test "/login" <|
                \() -> Routes.encode Routes.LoginPage
                |> Expect.equal "/login"
            ]

        , describe "decoding routes"
            [ test "/" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/" "" "" "" "")
                |> Expect.equal (Just Routes.PostsPage)
            , test "/post/{id}" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/post/5" "" "" "" "")
                |> Expect.equal (Just (Routes.PostPage 5))
            , test "/edit/{id}" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/edit/7" "" "" "" "")
                |> Expect.equal (Just (Routes.EditPostPage 7))
            , test "/new" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/new" "" "" "" "")
                |> Expect.equal (Just Routes.NewPostPage)
            , test "/login" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/login" "" "" "" "")
                |> Expect.equal (Just Routes.LoginPage)
            , test "/not-existing-path" <|
                \() -> Routes.decode (Location "" "" "" "" "" "" "/not-exist" "" "" "" "")
                |> Expect.equal Nothing
            ]
        ]
