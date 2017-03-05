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

instance ToJSON M.Post
instance FromJSON M.Post



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


type API = "post" :> PostAPI

combinedServer :: Sql.Connection -> Server API
combinedServer conn = postServer conn


api :: Proxy API
api = Proxy
