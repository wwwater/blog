module Util exposing (..)

import ServerApi

testPostEmpty : ServerApi.Post
testPostEmpty = ServerApi.Post Nothing (Just "") (Just "")

testPost : String -> ServerApi.Post
testPost content = ServerApi.Post
            (Just 1)
            (Just "Nice post")
            (Just content)
