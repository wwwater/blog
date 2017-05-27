{-# LANGUAGE OverloadedStrings #-}
module Storage where


import Data.Maybe               (fromMaybe)
import Database.SQLite.Simple   as Sql
import Data.Time.Clock.POSIX    (getPOSIXTime)

import qualified Model          as M


createSchema :: Sql.Connection -> IO ()
createSchema conn = do
  executeDB "PRAGMA foreign_keys = ON"
  executeDB "CREATE TABLE IF NOT EXISTS post\
            \(id INTEGER PRIMARY KEY ASC,\
            \title VARCHAR2(255),\
            \content TEXT,\
            \created INTEGER,\
            \published BOOLEAN)"
  executeDB "CREATE TABLE IF NOT EXISTS user (name VARCHAR2(255) PRIMARY KEY, password TEXT)"

  where
    executeDB = Sql.execute_ conn


getUtcTime :: IO Integer
getUtcTime = fmap round getPOSIXTime

selectAllPosts :: Sql.Connection -> Bool -> Maybe Int -> IO [M.Post]
selectAllPosts conn onlyPublished maybeOffset =
  Sql.queryNamed conn
    "SELECT id, title, content, created, published FROM post \
    \WHERE published >= :published ORDER BY created DESC LIMIT 10 OFFSET :offset"
    [
      ":published" := ((if onlyPublished then 1 else 0) :: Int),
      ":offset" := fromMaybe 0 maybeOffset
    ]
    :: IO [M.Post]

selectPost :: Sql.Connection -> Int -> IO (Maybe M.Post)
selectPost conn postId = do
  result <- (Sql.query conn "SELECT id, title, content, created, published FROM post WHERE id = ?"
            (Sql.Only postId) :: IO [M.Post])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ head result

insertPost :: Sql.Connection -> M.Post -> IO (Maybe M.Post)
insertPost conn post = do
  timeUtc <- getUtcTime
  Sql.executeNamed conn
    "INSERT INTO post (title, content, created, published) VALUES (:title, :content, :created, 0) "
    [":title" := (M.postTitle post), ":content" := (M.postContent post), ":created" := timeUtc]
  rawId <- lastInsertRowId conn
  insertedPost <- (Sql.query conn "SELECT id, title, content, created, published FROM post WHERE id = ?"
                  (Sql.Only $ rawId) :: IO [M.Post])
  case (length insertedPost) of
      0 -> return Nothing
      _ -> return $ Just $ head insertedPost

updatePost :: Sql.Connection -> M.Post -> IO (Maybe M.Post)
updatePost conn post = do
  Sql.executeNamed conn
    "UPDATE post SET title = :title, content = :content WHERE id = :id"
    [":id" := (M.postId post), ":title" := (M.postTitle post), ":content" := (M.postContent post)]
  updated <- (Sql.query conn "SELECT id, title, content, created, published FROM post WHERE id = ?"
             (Sql.Only $ M.postId post) :: IO [M.Post])
  case (length updated) of
      0 -> return Nothing
      _ -> return $ Just $ head updated

publishPost :: Sql.Connection -> Int -> IO (Maybe M.Post)
publishPost conn postId = do
  Sql.executeNamed conn "UPDATE post SET published = 1 WHERE id = :id" [":id" := postId]
  updated <- (Sql.query conn "SELECT id, title, content, created, published FROM post WHERE id = ?"
             (Sql.Only postId) :: IO [M.Post])
  case (length updated) of
      0 -> return Nothing
      _ -> return $ Just $ head updated

deletePost :: Sql.Connection -> Int -> IO ()
deletePost conn postId = do
  Sql.execute conn "DELETE FROM post WHERE id = ?" (Sql.Only postId)


getUserPassword :: Sql.Connection -> String -> IO (Maybe String)
getUserPassword conn username = do
  result <- (Sql.query conn "SELECT name, password FROM user WHERE name = ?"
            (Sql.Only username) :: IO [M.Credentials])
  case (length result) of
      0 -> return Nothing
      _ -> return $ Just $ M.password $ head result

