{-# LANGUAGE OverloadedStrings #-}


module ApiSpec (main, spec) where

import Test.Hspec               (hspec, Spec, beforeAll, afterAll, after, describe, it)
import Test.Hspec.Wai           (get, request, shouldRespondWith, matchStatus, WaiSession (..))
import Network.Wai.Test         (SResponse (..))
import Test.Hspec.Wai.Internal  (withApplication)
import Crypto.PasswordStore     (makePassword)
import Data.Text.Encoding       (decodeUtf8)
import Data.Aeson               (decode, encode)

import qualified Database.SQLite.Simple as Sql
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as Char8
import qualified Data.ByteString.Lazy   as BL

import qualified App
import qualified Storage
import qualified Model



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

    it "retrieves only published posts" $ \connection -> do
      Storage.createSchema connection
      Sql.execute_ connection "INSERT INTO post (id, title, content, published) VALUES (1, 'Title', 'Content', 1)"
      Sql.execute_ connection "INSERT INTO post (id, title, content, published) VALUES (2, 'Title', 'Content', 0)"
      withApplication (App.app connection) $ do
        get "/post" `shouldRespondWith`
          "[{\"createdAt\":null,\"published\":true,\"postContent\":\"Content\",\"postId\":1,\"postTitle\":\"Title\"}]"
          {matchStatus = 200}

    it "retrieves published posts with offset" $ \connection -> do
      Storage.createSchema connection
      Sql.execute_ connection "INSERT INTO post (id, title, content, published) VALUES (1, 'Title', 'Content', 1)"
      Sql.execute_ connection "INSERT INTO post (id, title, content, published) VALUES (2, 'Title', 'Content', 1)"
      withApplication (App.app connection) $ do
        get "/post?offset=1" `shouldRespondWith`
          "[{\"createdAt\":null,\"published\":true,\"postContent\":\"Content\",\"postId\":2,\"postTitle\":\"Title\"}]"
          {matchStatus = 200}

    it "retrieves a post by id" $ \connection -> do
      Storage.createSchema connection
      Sql.execute_ connection "INSERT INTO post (id, title, content, published) VALUES (3, 'Title', 'Content', 1)"
      Sql.execute_ connection "INSERT INTO post (id, title, content, created, published) VALUES (5, 'Title', 'Content',123, 1)"
      withApplication (App.app connection) $ do
        get "/post/5" `shouldRespondWith`
          "{\"createdAt\":123,\"published\":true,\"postContent\":\"Content\",\"postId\":5,\"postTitle\":\"Title\"}"
          {matchStatus = 200}

    it "returns 404 if post is not published" $ \connection -> do
      Storage.createSchema connection
      Sql.execute_ connection "INSERT INTO post (id, title, content, published) VALUES (4, 'Title', 'Content', 0)"
      withApplication (App.app connection) $ do
        get "/post/4" `shouldRespondWith` 404

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
                (createPostJson Nothing "New post" "Content")
        `shouldRespondWith`401


  describe "test POST /jwt endpoint" $
    after (\connection -> Sql.execute_ connection "DROP TABLE user") $ do

    it "retrieves a JWT" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                (createCredentialsJson "test" "testPassword")
            `shouldRespondWith` 200

    it "returns an error if user does not exist" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                (createCredentialsJson "not-existent-user" "testPassword")
            `shouldRespondWith` 401

    it "returns an error if password is wrong" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        request "POST"
                "/jwt"
                [("Content-Type", "application/json")]
                (createCredentialsJson "test" "wrongPassword")
            `shouldRespondWith` 401


  describe "test /post endpoints that require JWT" $
    after (\connection -> Sql.execute_ connection "DROP TABLE user") $ do

    it "can retrieve unpublished posts using JWT" $ \connection -> do
      Sql.execute_ connection "INSERT INTO post (id, title, content, published) VALUES (1, 'Title', 'Content', 0)"
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        response <- makeJwtRequest
        request "GET"
                "/post"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
          `shouldRespondWith`
          "[{\"createdAt\":null,\"published\":false,\"postContent\":\"Content\",\"postId\":1,\"postTitle\":\"Title\"}]"
          {matchStatus = 200}

    it "can retrieve unpublished post using JWT" $ \connection -> do
      Sql.execute_ connection "INSERT INTO post (id, title, content) VALUES (2, 'Title', 'Content')"
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        response <- makeJwtRequest
        request "GET"
                "/post/2"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                ""
          `shouldRespondWith`
          "{\"createdAt\":null,\"published\":null,\"postContent\":\"Content\",\"postId\":2,\"postTitle\":\"Title\"}"
          {matchStatus = 200}

    it "can create a new post using JWT" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        response <- makeJwtRequest
        request "POST"
                "/post"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                (createPostJson Nothing "New post" "Content")
          `shouldRespondWith` 200

    it "can update a post using JWT" $ \connection -> do
      addTestUserToDB connection
      Sql.execute_ connection "INSERT OR IGNORE INTO post (id, title, content) VALUES (3, 'Title', 'Content')"
      withApplication (App.app connection) $ do
        response <- makeJwtRequest
        request "POST"
                "/post"
                [("Content-Type", "application/json"), ("jwt", getJwtFromResponse response)]
                (createPostJson (Just 3) "Updated post" "Content")
          `shouldRespondWith` 200

    it "can delete a post using JWT" $ \connection -> do
      addTestUserToDB connection
      Sql.execute_ connection "INSERT OR IGNORE INTO post (id, title, content) VALUES (3, 'Title', 'Content')"
      withApplication (App.app connection) $ do
        jwtResponse <- makeJwtRequest
        _ <- request
          "DELETE"
          "/post/3"
          [("Content-Type", "application/json"), ("jwt", getJwtFromResponse jwtResponse)]
          ""
        get "/post/3" `shouldRespondWith` 404

    it "can publish a post using JWT" $ \connection -> do
      addTestUserToDB connection
      Sql.execute_ connection "INSERT OR IGNORE INTO post (id, title, content) VALUES (3, 'Title', 'Content')"
      withApplication (App.app connection) $ do
        jwtResponse <- makeJwtRequest
        request
          "POST"
          "/post/3/publish"
          [("Content-Type", "application/json"), ("jwt", getJwtFromResponse jwtResponse)]
          ""
        `shouldRespondWith` 200

    it "publishing a post with wrong id returns 404" $ \connection -> do
      addTestUserToDB connection
      withApplication (App.app connection) $ do
        jwtResponse <- makeJwtRequest
        request
          "POST"
          "/post/33/publish"
          [("Content-Type", "application/json"), ("jwt", getJwtFromResponse jwtResponse)]
          ""
        `shouldRespondWith` 404

    it "cannot create a new post using wrong JWT" $ \connection -> do
      Storage.createSchema connection
      withApplication (App.app connection) $ do
        request "POST"
                "/post"
                [("Content-Type", "application/json"), ("jwt", "wrongJWT")]
                (createPostJson Nothing "New post" "Content")
          `shouldRespondWith` 401


addTestUserToDB :: Sql.Connection -> IO()
addTestUserToDB connection = do
  Storage.createSchema connection
  hash <- makePassword "testPassword" 17
  Sql.executeNamed connection "INSERT INTO user (name, password) VALUES ('test', :hash)"
    [":hash" Sql.:= decodeUtf8 hash]

createPostJson :: Maybe Int -> String -> String -> BL.ByteString
createPostJson id title content =
    encode (Model.Post {
        Model.postId = id
      , Model.postTitle = Just title
      , Model.postContent = Just content
      , Model.createdAt = Nothing
      , Model.published = Just False
      })

createCredentialsJson :: String -> String -> BL.ByteString
createCredentialsJson user password =
    encode (Model.Credentials {Model.username = user, Model.password = password})


getJwtFromResponse :: SResponse -> B.ByteString
getJwtFromResponse response =
  case decode (simpleBody response) of
    Just jwt -> Char8.pack $ Model.token jwt
    Nothing -> ""

makeJwtRequest :: WaiSession SResponse
makeJwtRequest = request
  "POST"
  "/jwt"
  [("Content-Type", "application/json")]
  (createCredentialsJson "test" "testPassword")

