{-# LANGUAGE OverloadedStrings #-}
module Bootstrap where

import Database.SQLite.Simple as Sql


bootstrapDB :: Sql.Connection -> IO ()
bootstrapDB conn = do
  createSchema conn
  populateSampleData conn


createSchema :: Sql.Connection -> IO ()
createSchema conn = do
  executeDB "PRAGMA foreign_keys = ON"
  executeDB "CREATE TABLE IF NOT EXISTS post (id INTERGER PRIMARY KEY ASC, title VARCHAR2(255), content TEXT)"
  executeDB "CREATE TABLE IF NOT EXISTS user (name INTERGER PRIMARY KEY ASC, password TEXT)"

  where
    executeDB = Sql.execute_ conn

posts :: [(Int, String, String)]
posts = [ (1, "Test post", "This is a test post")
        , (2, "Test post 2", "This is a test post number two, maybe it it a bit longer than the first one")
        ]



populateSampleData :: Sql.Connection -> IO ()
populateSampleData conn = do
  mapM_ insertPost posts

  where
    insertPost p = Sql.execute conn "INSERT OR IGNORE INTO post (id, title, content) VALUES (?, ?, ?)" p
