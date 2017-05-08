{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where


import Servant                              (Proxy (..), throwError)
import Servant.Server                       (Server, Handler)
import Servant.API.Alternative              ((:<|>) (..))
import Servant.API.Sub                      ((:>))
import Servant.API.Verbs                    (Get, Post, DeleteNoContent)
import Servant.API.ReqBody                  (ReqBody)
import Servant.API.Capture                  (Capture)
import Servant.API.ContentTypes             (JSON)
import Servant.API.Header                   (Header)
import Servant.Server.Internal.ServantErr   (ServantErr, err401, err404, errBody)
import Control.Monad.IO.Class               (liftIO)
import Database.SQLite.Simple as Sql
import Crypto.PasswordStore                 (verifyPassword)
import Data.ByteString.Char8                (pack)
import Data.Text                            (unpack)
import System.Environment                   (lookupEnv)
import Data.Maybe                           (fromMaybe)

import Jwt                                  (createJwt, verifyJwt)
import qualified Model as M
import qualified Storage as S


defaultJwtSecret :: String
defaultJwtSecret = "jwt-secret"


type JwtAPI =
  ReqBody '[JSON] M.Credentials :> Post '[JSON] M.Jwt

jwtServer :: Sql.Connection -> Server JwtAPI
jwtServer conn =
  grantJwt
    where
      grantJwt :: M.Credentials -> Handler M.Jwt
      grantJwt credentials = liftIOMaybeToHandler err (jwt credentials)
      err = err401 { errBody = "Wrong password or user does not exist."}
      jwt credentials = issueJwt (M.password credentials) (passwordHash credentials)
      passwordHash credentials = S.getUserPassword conn (M.username credentials)

issueJwt :: String -> IO (Maybe String) -> IO (Maybe M.Jwt)
issueJwt password passwordHashIO = do
  maybePasswordHash <- liftIO passwordHashIO
  case maybePasswordHash of
    Just passwordHash ->
      if verifyPassword (pack password) (pack passwordHash)
        then do
          jwtSecret <- lookupEnv "JWT_SECRET"
          let secret = fromMaybe defaultJwtSecret jwtSecret in
            fmap (Just . M.Jwt . unpack) $ createJwt secret
        else return Nothing
    Nothing -> return Nothing




type PostAPI =
       Get '[JSON] [M.Post]
  :<|> Capture "postId" Int :> Get '[JSON] M.Post
  :<|> Header "jwt" String :> ReqBody '[JSON] M.Post :> Post '[JSON] M.Post
  :<|> Header "jwt" String :> Capture "postId" Int :> "publish" :> Post '[JSON] M.Post
  :<|> Header "jwt" String :> Capture "postId" Int :> DeleteNoContent '[JSON] ()


postServer :: Sql.Connection -> Server PostAPI
postServer conn =
  getAllPosts :<|> getPost :<|> updatePost :<|> publishPost :<|> deletePost

  where
    getAllPosts = liftIO $ S.selectAllPosts conn
    getPost postId = liftIOMaybeToHandler err404 $ S.selectPost conn postId
    updatePost jwt post = wrapInJwtCheck jwt $ updateAuthorizedPost conn post
    publishPost jwt postId = wrapInJwtCheck jwt $ liftIOMaybeToHandler err404 $ S.publishPost conn postId
    deletePost jwt postId = wrapInJwtCheck jwt $ liftIO $ S.deletePost conn postId


wrapInJwtCheck :: Maybe String -> Handler a -> Handler a
wrapInJwtCheck jwt callback =
  case jwt of
    Just jwtToken -> do
      jwtSecret <- liftIO $ lookupEnv "JWT_SECRET"
      let secret = fromMaybe defaultJwtSecret jwtSecret in do
        valid <- liftIO $ verifyJwt secret jwtToken
        if valid
          then callback
          else throwError err401 { errBody = "JWT token has expired or not valid." }
    Nothing -> throwError err401 { errBody = "Please provide JWT token in header." }


updateAuthorizedPost :: Sql.Connection -> M.Post -> Handler M.Post
updateAuthorizedPost conn post = liftIOMaybeToHandler err404 $
  case M.postId post of
    Just _ -> S.updatePost conn post
    Nothing -> S.insertPost conn post





liftIOMaybeToHandler :: ServantErr -> IO (Maybe a) -> Handler a
liftIOMaybeToHandler err x = do
  m <- liftIO x
  case m of
    Nothing -> throwError err
    Just y -> return y






type API =
       "jwt" :> JwtAPI
  :<|> "post" :> PostAPI

combinedServer :: Sql.Connection -> Server API
combinedServer conn =
       jwtServer conn
  :<|> postServer conn


api :: Proxy API
api = Proxy
