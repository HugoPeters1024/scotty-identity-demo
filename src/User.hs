{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module User where

import GHC.Generics
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson (ToJSON)
import Auth (IsUser, getUsername, getPassword)
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

data User = User {
    u_userId :: Int
  , u_userName :: String
  , u_password :: String
} deriving (Show, Generic)

instance ToJSON User
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance IsUser User where
  getUsername = T.pack . u_userName
  getPassword = T.pack . u_password


