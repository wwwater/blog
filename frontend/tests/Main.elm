port module Main exposing (..)

import PostsTest
import PostTest
import EditPostTest
import RoutesTest
import LoginTest
import MenuTest
import Test             exposing (concat)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode      exposing (Value)


main : TestProgram
main =
    run emit (Test.concat
        [ PostsTest.all
        , PostTest.all
        , EditPostTest.all
        , RoutesTest.all
        , LoginTest.all
        , MenuTest.all
        ])


port emit : ( String, Value ) -> Cmd msg
