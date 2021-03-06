module TestUtils exposing (..)

import ServerApi

testPostEmpty : ServerApi.Post
testPostEmpty = ServerApi.Post
    Nothing
    (Just "")
    (Just "")
    Nothing
    Nothing

testPost : String -> ServerApi.Post
testPost content = ServerApi.Post
    (Just 1)
    (Just "Nice post")
    (Just content)
    (Just 1492179534)
    (Nothing)
