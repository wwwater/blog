{-# LANGUAGE OverloadedStrings #-}
module Storage where


import qualified Model as M
import qualified Data.Text as Txt
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Maybe


import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes


instance Sql.FromRow M.Post where
  fromRow = M.Post <$> Sql.field <*> Sql.field <*> Sql.field



selectAllPosts :: Sql.Connection -> IO [M.Post]
selectAllPosts conn =
  Sql.query_ conn "select * from post" :: IO [M.Post]

selectPost :: Sql.Connection -> Int -> IO (Maybe M.Post)
selectPost conn postId = do
  result <- (Sql.query conn "select * from post where id = ?" (Sql.Only postId) :: IO [M.Post])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ head result

addPost :: Sql.Connection -> M.Post -> IO M.Post
addPost conn post = do
  Sql.execute conn "insert into post (title) values (?) " (Sql.Only $ M.postTitle post)
  rawId <- lastInsertRowId conn
  let newPost = post { M.postId = fromIntegral rawId }
  return newPost

