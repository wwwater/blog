{-# LANGUAGE OverloadedStrings #-}

module ApiSpec (main, spec) where

import Test.Hspec               (hspec, Spec, beforeAll, afterAll, describe, it)
import Test.Hspec.Wai           (get, shouldRespondWith, matchStatus)
import Test.Hspec.Wai.Internal  (withApplication)

import qualified Database.SQLite.Simple as Sql

import qualified App
import qualified Bootstrap


main :: IO ()
main = hspec spec


testConnect :: IO Sql.Connection
testConnect = Sql.open ":memory:"


spec :: Spec
spec = beforeAll testConnect $ afterAll Sql.close $ do
  describe "test GET /post endpoint" $ do

    it "responds with 200 and empty array" $ \connection -> do
      Bootstrap.createSchema connection
      withApplication (App.app connection) $ do
        get "/post" `shouldRespondWith` "[]" {matchStatus = 200}

    it "responds with 200 and one post" $ \connection -> do
      Bootstrap.createSchema connection
      Sql.execute_ connection "INSERT INTO post (id, title, content) VALUES (1, 'Title', 'Content')"
      withApplication (App.app connection) $ do
        get "/post" `shouldRespondWith`
          "[{\"postContent\":\"Content\",\"postId\":1,\"postTitle\":\"Title\"}]"
          {matchStatus = 200}

