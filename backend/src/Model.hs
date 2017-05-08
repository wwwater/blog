{-# LANGUAGE DeriveGeneric #-}

module Model where

import GHC.Generics

import Database.SQLite.Simple     as Sql
import Data.Aeson                 (FromJSON, ToJSON)

data Post = Post
  { postId :: Maybe Int
  , postTitle :: Maybe String
  , postContent :: Maybe String
  , createdAt :: Maybe Int
  , published :: Maybe Bool
  } deriving (Generic)

data Credentials = Credentials
  { username :: String
  , password :: String
  } deriving (Generic)

data Jwt = Jwt
  { token :: String
  } deriving (Generic)


instance Sql.FromRow Post where
  fromRow = Post <$> Sql.field <*> Sql.field <*> Sql.field <*> Sql.field <*> Sql.field

instance Sql.FromRow Credentials where
  fromRow = Credentials <$> Sql.field <*> Sql.field

instance FromJSON Credentials
instance ToJSON Credentials
instance FromJSON Jwt
instance ToJSON Jwt
instance FromJSON Post
instance ToJSON Post
