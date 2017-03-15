{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import qualified Model as M
import qualified Storage as S

import Data.Aeson
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Trans.Either
import Servant
import Servant.Server.Internal.ServantErr   (ServantErr, err401, err404)
import Database.SQLite.Simple as Sql
import Crypto.PasswordStore                 (verifyPassword)
import Data.ByteString.Char8                (pack)
import Data.Text                            (unpack)

import Jwt                                  (createJwt, verifyJwt)

instance FromJSON M.Credentials
instance ToJSON M.Jwt
instance FromJSON M.Post
instance ToJSON M.Post




type JwtAPI =
  ReqBody '[JSON] M.Credentials :> Post '[JSON] M.Jwt

jwtServer :: Sql.Connection -> Server JwtAPI
jwtServer conn =
  grantJwt
    where
      passwordHash credentials = S.getUserPassword conn (M.username credentials)
      jwt credentials = issueJwt (M.password credentials) (passwordHash credentials)
      error = err401 { errBody = "Wrong password or user does not exist."}
      grantJwt credentials = liftIOMaybeToEither error (jwt credentials)

issueJwt :: String -> IO (Maybe String) -> IO (Maybe M.Jwt)
issueJwt password passwordHashIO = do
  maybePasswordHash <- liftIO passwordHashIO
  case maybePasswordHash of
    Just passwordHash ->
      if verifyPassword (pack password) (pack passwordHash)
        then fmap (Just . M.Jwt . unpack) $ createJwt "meow"
        else return Nothing
    Nothing -> return Nothing





type PostAPI =
       Get '[JSON] [M.Post]
  :<|> Capture "postId" Int :> Get '[JSON] M.Post
  :<|> Header "jwt" String :> ReqBody '[JSON] M.Post :> Post '[JSON] M.Post


postServer :: Sql.Connection -> Server PostAPI
postServer conn =
  getAllPosts :<|> getPost :<|> updatePost

  where
    getAllPosts = liftIO $ S.selectAllPosts conn
    getPost postId = liftIOMaybeToEither err404 $ S.selectPost conn postId
    updatePost jwt post =
      case jwt of
        Just jwtToken -> do
          valid <- liftIO $ verifyJwt "meow" jwtToken
          if valid
            then updateAuthorizedPost conn post
            else left err401 { errBody = "JWT token has expired or not valid." }
        Nothing -> left err401 { errBody = "Please provide JWT token in header." }



updateAuthorizedPost :: Sql.Connection -> M.Post -> EitherT ServantErr IO M.Post
updateAuthorizedPost conn post = liftIOMaybeToEither err404 $
  case M.postId post of
    Just postId -> S.updatePost conn post
    Nothing -> S.insertPost conn post






liftIOMaybeToEither ::  (MonadIO m) => a -> IO (Maybe b) -> EitherT a m b
liftIOMaybeToEither err x = do
  m <- liftIO x
  case m of
    Nothing -> left err
    Just x -> right x






type API =
       "jwt" :> JwtAPI
  :<|> "post" :> PostAPI

combinedServer :: Sql.Connection -> Server API
combinedServer conn =
       jwtServer conn
  :<|> postServer conn


api :: Proxy API
api = Proxy
