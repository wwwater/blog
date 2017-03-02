{-# LANGUAGE DeriveGeneric #-}

module Model where

import GHC.Generics


data Comment = Comment
   { commentId :: Int
   , commentAuthor :: Maybe String
   , commentContent :: String
   } deriving (Eq, Show, Generic)

data Post = Post
   { postId :: Int
   , postTitle :: Maybe String
   , postContent :: Maybe String
   } deriving (Eq, Show, Generic)

