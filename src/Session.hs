{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Session where

import GHC.Generics
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.Aeson (ToJSON)
import Data.UUID
import qualified Data.Text as T
import Auth

data Session = Session {
    s_id :: Int
  , s_sessionKey :: UUID
  , s_userId :: Int
} deriving (Show, Generic)

instance ToJSON Session
instance FromRow Session where
  fromRow = Session <$> field <*> field <*> field
instance ToRow Session

instance IsSession Session where
  getIdentifier = s_sessionKey 


