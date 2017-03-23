port module Main exposing (..)

import PostsTest
import PostTest
import RoutesTest
import Test             exposing (concat)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode      exposing (Value)


main : TestProgram
main =
    run emit (Test.concat
        [ PostsTest.all
        , PostTest.all
        , RoutesTest.all
        ])


port emit : ( String, Value ) -> Cmd msg
