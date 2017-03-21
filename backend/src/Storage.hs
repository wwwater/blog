{-# LANGUAGE OverloadedStrings #-}
module Storage where


import qualified Model as M
import Database.SQLite.Simple as Sql


createSchema :: Sql.Connection -> IO ()
createSchema conn = do
  executeDB "PRAGMA foreign_keys = ON"
  executeDB "CREATE TABLE IF NOT EXISTS post (id INTERGER PRIMARY KEY ASC, title VARCHAR2(255), content TEXT)"
  executeDB "CREATE TABLE IF NOT EXISTS user (name INTERGER PRIMARY KEY ASC, password TEXT)"

  where
    executeDB = Sql.execute_ conn




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


getUserPassword :: Sql.Connection -> String -> IO (Maybe String)
getUserPassword conn username = do
  result <- (Sql.query conn "SELECT * FROM user WHERE name = ?" (Sql.Only username) :: IO [M.Credentials])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ M.password $ head result

