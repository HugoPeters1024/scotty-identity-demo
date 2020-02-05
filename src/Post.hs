{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Post where

import GHC.Generics
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson (ToJSON)


data Post = Post {
    p_postId :: Int,
    p_userId :: Int,
    p_post   :: String
} deriving (Show, Generic)

instance ToJSON Post
instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field

