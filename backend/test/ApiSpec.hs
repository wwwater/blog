{-# LANGUAGE OverloadedStrings #-}

module ApiSpec (main, spec) where

import Test.Hspec               (hspec, Spec, beforeAll, afterAll, after, describe, it)
import Test.Hspec.Wai           (get, request, shouldRespondWith, matchStatus)
import Network.Wai.Test         (SResponse (..))
import Test.Hspec.Wai.Internal  (withApplication)
import Crypto.PasswordStore     (makePassword)
import Data.Text.Encoding       (decodeUtf8)

import qualified Database.SQLite.Simple as Sql
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL

import qualified App
import qualified Storage



main :: IO ()
main = hspec spec


testConnect :: IO Sql.Connection
testConnect = Sql.open ":memory:"


spec :: Spec
spec = beforeAll testConnect $
       afterAll Sql.close $ do

  describe "test GET /post endpoint" $
    after (\connection -> Sql.execute_ connection "DROP TABLE post") $ do

    it "retrieves empty list of posts" $ \connection -> do
      Storage.createSchema connection
      withApplication (App.app connection) $ do
        get "/post" `shouldRespondWith` "[]" {matchStatus = 200}

    it "retrieves one post" $ \connection -> do
      Storage.createSchema connection
      Sql.execute_ connection "INSERT INTO post (id, title, content) VALUES (1, 'Title', 'Content')"
      withApplication (App.app connection) $ do
        get "/post" `shouldRespondWith`
          "[{\"postContent\":\"Content\",\"postId\":1,\"postTitle\":\"Title\"}]"
          {matchStatus = 200}

    it "retrieves a post by id" $ \connection -> do
      Storage.createSchema connection
      Sql.execute_ connection "INSERT INTO post (id, title, content) VALUES (3, 'Title', 'Content')"
      Sql.execute_ connection "INSERT INTO post (id, title, content) VALUES (5, 'Title', 'Content')"
      withApplication (App.app connection) $ do
        get "/post/5" `shouldRespondWith`
          "{\"postContent\":\"Content\",\"postId\":5,\"postTitle\":\"Title\"}"
          {matchStatus = 200}

    it "returns 404 if post id does not exist" $ \connection -> do
      Storage.createSchema connection
      withApplication (App.app connection) $ do
        get "/post/5" `shouldRespondWith`404

    it "does not permit update a post without JWT" $ \connection -> do
      Storage.createSchema connection
      withApplication (App.app connection) $ do
        request "POST"
                "/post"
                [("Content-Type", "application/json")]
                "{\"postContent\":\"Content\",\"postTitle\":\"New post\"}"
        `shouldRespondWith`401


  describe "test POST /jwt endpoint" $
    after (\connection -> Sql.execute_ connection "DROP TABLE user") $ do

    it "retrieves a JWT" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                "{\"username\":\"test\",\"password\":\"testPassword\"}"
            `shouldRespondWith` 200

    it "returns an error if user does not exist" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                "{\"username\":\"test-not-exist\",\"password\":\"testPassword\"}"
            `shouldRespondWith` 401

    it "returns an error if password is wrong" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                "{\"username\":\"test\",\"password\":\"testPasswordWrong\"}"
            `shouldRespondWith` 401

    it "can create a new post using JWT" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        response <- request "POST"
                            "/jwt"
                            [("Content-Type", "application/json")]
                            "{\"username\":\"test\",\"password\":\"testPassword\"}"
        request "POST"
                "/post"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                "{\"postContent\":\"Content\",\"postTitle\":\"New post\"}"
          `shouldRespondWith` "{\"postContent\":\"Content\",\"postId\":1,\"postTitle\":\"New post\"}"

    it "can create update a post using JWT" $ \connection -> do
      addTestUserToDB connection
      Sql.execute_ connection "INSERT INTO post (id, title, content) VALUES (3, 'Title', 'Content')"
      withApplication (App.app connection) $ do
        response <- request "POST"
                            "/jwt"
                            [("Content-Type", "application/json")]
                            "{\"username\":\"test\",\"password\":\"testPassword\"}"
        request "POST"
                "/post"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                "{\"postContent\":\"Content\",\"postId\":3,\"postTitle\":\"Updated post\"}"
          `shouldRespondWith` "{\"postContent\":\"Content\",\"postId\":3,\"postTitle\":\"Updated post\"}"

    it "cannot create a new post using wrong JWT" $ \connection -> do
      Storage.createSchema connection
      withApplication (App.app connection) $ do
        request "POST"
                "/post"
                [("Content-Type", "application/json"), ("jwt", "wrongJWT")]
                "{\"postContent\":\"Content\",\"postTitle\":\"New post\"}"
          `shouldRespondWith` 401


addTestUserToDB :: Sql.Connection -> IO()
addTestUserToDB connection = do
  Storage.createSchema connection
  hash <- makePassword "testPassword" 17
  Sql.executeNamed connection "INSERT INTO user (name, password) VALUES ('test', :hash)"
    [":hash" Sql.:= decodeUtf8 hash]


getJwtFromResponse :: SResponse -> B.ByteString
getJwtFromResponse response =
    let body = B.concat . BL.toChunks $ simpleBody response
        in B.reverse $ B.drop 2 $ B.reverse $ B.drop 10 body

