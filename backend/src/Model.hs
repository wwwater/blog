{-# LANGUAGE DeriveGeneric #-}

module Model where

import GHC.Generics


data Comment = Comment
  { commentId :: Int
  , commentAuthor :: Maybe String
  , commentContent :: String
  } deriving (Eq, Show, Generic)

data Post = Post
  { postId :: Maybe Int
  , postTitle :: Maybe String
  , postContent :: Maybe String
  } deriving (Eq, Show, Generic)

data Credentials = Credentials
  { username :: String
  , password :: String
  } deriving (Eq, Show, Generic)

data Jwt = Jwt
  { token :: String
  } deriving (Eq, Show, Generic)

