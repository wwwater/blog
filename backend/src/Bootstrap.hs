{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Bootstrap where

import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes


bootstrapDB :: Sql.Connection -> IO ()
bootstrapDB conn = do
  createSchema conn
  populateSampleData conn


createSchema :: Sql.Connection -> IO ()
createSchema conn = do
  executeDB "PRAGMA foreign_keys = ON"
  executeDB "create table post (id integer primary key asc, title varchar2(255), content text)"

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
    insertPost p = Sql.execute conn "insert into post (id, title, content) values (?, ?, ?)" p
