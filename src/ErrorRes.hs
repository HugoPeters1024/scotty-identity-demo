{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ErrorRes where

import GHC.Generics
import Web.Scotty
import Data.Aeson (ToJSON)
import Text.Blaze.Html5 as H hiding (html)
import Text.Blaze.Html.Renderer.Text as R


data ErrorRes = ErrorRes {
    errId :: Int
  , errDecr :: String
} deriving (Show, Generic)

instance ToJSON ErrorRes

tryResJson :: ToJSON a => Maybe a -> ErrorRes -> ActionM ()
tryResJson Nothing err = json err
tryResJson (Just x) _  = json x

tryResHtml :: Maybe Html -> ErrorRes -> ActionM ()
tryResHtml Nothing err = json err
tryResHtml (Just x) _  = html $ renderHtml x
