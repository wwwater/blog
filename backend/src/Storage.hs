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
  result <- (Sql.query conn "SELECT * FROM post WHERE id = ?" (Sql.Only postId) :: IO [M.Post])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ head result

insertPost :: Sql.Connection -> M.Post -> IO (Maybe M.Post)
insertPost conn post = do
  Sql.executeNamed conn "INSERT INTO post (title, content) VALUES (:title, :content) " [":title" := (M.postTitle post), ":content" := (M.postContent post)]
  rawId <- lastInsertRowId conn
  let newPost = post { M.postId = Just $ fromIntegral rawId }
  return (Just newPost)

updatePost :: Sql.Connection -> M.Post -> IO (Maybe M.Post)
updatePost conn post = do
  Sql.executeNamed conn "UPDATE post SET title = :title, content = :content WHERE id = :id" [":id" := (M.postId post), ":title" := (M.postTitle post), ":content" := (M.postContent post)]
  updated <- (Sql.query conn "SELECT * FROM post WHERE id = ?" (Sql.Only $ M.postId post) :: IO [M.Post])
  case (length updated) of
      0 -> return Nothing
      _ -> return $ Just $ head updated

