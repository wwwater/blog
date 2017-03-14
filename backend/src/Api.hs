{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Api where

import qualified Model as M
import qualified Storage as S

import Data.Aeson
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Either
import Servant
import Database.SQLite.Simple as Sql
import Crypto.PasswordStore (verifyPassword)
import Data.ByteString.Char8 (pack)
import Data.Text             (unpack)

import Jwt

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
      grantJwt credentials = liftIOMaybeToEither err401 $
        issueJwt (M.password credentials) $ S.getUserPassword conn (M.username credentials)

issueJwt :: String -> IO (Maybe String) -> IO (Maybe M.Jwt)
issueJwt password hashIO = do
    hash <- liftIO hashIO
    case hash of
        Just passwordHash ->
            if verifyPassword (pack password) (pack passwordHash)
                then fmap (Just . M.Jwt . unpack) $ createJwt "meow"
                else return Nothing
        Nothing -> return Nothing





type PostAPI =
       Get '[JSON] [M.Post]
  :<|> Capture "postId" Int :> Get '[JSON] M.Post
  :<|> ReqBody '[JSON] M.Post :> Post '[JSON] M.Post


postServer :: Sql.Connection -> Server PostAPI
postServer conn =
  getAllPosts :<|> getPost :<|> updatePost

  where
    getAllPosts = liftIO $ S.selectAllPosts conn
    getPost postId = liftIOMaybeToEither err404 $ S.selectPost conn postId
    updatePost post = liftIOMaybeToEither err404 $
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
