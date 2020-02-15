{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module User where

import GHC.Generics
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson (ToJSON)
import Web.Scotty.Identity (IsUser, getUsername, getPassword)
import qualified Data.ByteString as BT
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Text.Hex


data User = User {
    u_userId :: Int
  , u_userName :: T.Text
  , u_password :: BT.ByteString
} deriving (Show, Generic)

instance FromRow User where
  fromRow = User <$> field <*> field <*> ((fromMaybe "" . decodeHex) <$> field)

instance IsUser User where
  getUsername = u_userName
  getPassword = u_password


