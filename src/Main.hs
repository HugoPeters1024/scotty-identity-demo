{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Control.Monad (liftM) 
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import qualified Data.Vault.Lazy as V
import Web.Scotty
import Web.Scotty.Cookie
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Aeson (FromJSON, ToJSON, encode, decode, parseJSON, withObject, (.:))
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived, vault, strictRequestBody, lazyRequestBody)
import Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4

import User
import Post
import ErrorRes
import UserPage
import Auth
import Session

{-
data RequestData = RequestData {
  rd_userName :: String
} deriving Show

instance FromJSON RequestData where
  parseJSON = withObject "RequestData" $ \v -> RequestData
    <$> v .: "username"
-}

data RequestData = RequestData UUID deriving (Show, Generic)

instance FromJSON RequestData where
  parseJSON = withObject "RequestData" $ \v -> RequestData
    <$> v .: "uuid"


test :: Maybe RequestData
test = decode "{\"username\":\"lol\"}"

main :: IO ()
main = do
  conn <- connect defaultConnectInfo {
      connectHost = "localhost",
      connectPort = 5432,
      connectUser = "postgres",
      connectPassword = "mysecretpassword",
      connectDatabase = "haskell" 
  }
  let gu = getUser conn
  let vu = \u p -> True
  (mm, mkAuth, routeLogin) <- useAuth readSession (getSession conn) verifySession (sessionToUser conn) (retrieveUser conn) (createSession conn)
  scotty 3000 $ do
    middleware mm
    get "/" $ do
      hits <- liftM (fromMaybe "") $ getCookie "hits"
      let hits' = T.append hits "t"
      setSimpleCookie "hits" hits'
      text $ LT.fromStrict hits'
    get "/user"   $ mkAuth $ routeUser conn
    post "/login" $ routeLogin

readSession :: Request -> IO (Maybe RequestData)
readSession req = lazyRequestBody req >>= (return . decode)

getSession :: Connection -> RequestData -> IO (Maybe Session)
getSession conn (RequestData key) = headM <$> query conn "select * from sessions where key = ?" [key]

verifySession :: Session -> IO Bool
verifySession _ = return True

sessionToUser :: Connection -> Session -> IO (Maybe User)
sessionToUser conn session = getUser conn (s_userId session)

retrieveUser :: Connection -> Username -> IO (Maybe User)
retrieveUser conn uname = headM <$> query conn "select * from users where name = ?" [uname]

createSession :: Connection -> User -> IO Session
createSession conn user = do 
  let user_id = u_userId user
  uuid <- nextRandom
  mq <- headM <$> query conn "insert into sessions (key, user_id) values (?, ?) returning id" (uuid, user_id) :: IO (Maybe (Only Int))
  let q = case mq of
        Nothing -> error "cannot create session"
        Just (Only x)  -> x
  return (Session q uuid 0)


routeUser :: Connection -> User -> ActionM ()
routeUser conn user = do
    posts <- liftAndCatchIO $ getUserPosts conn (u_userId user)
    html $ renderHtml $ userPage user posts

getUser :: Connection -> Int -> IO (Maybe User)
getUser conn id = headM <$> query conn "select * from users where user_id = ?" [id]

getPosts :: Connection -> IO (Maybe Post)
getPosts conn = headM <$> query_ conn "select * from posts"

getUserPosts :: Connection -> Int -> IO [Post]
getUserPosts conn id = query conn "select * from posts where user_id = ?" [id]

headM :: [a] -> Maybe a
headM []    = Nothing
headM (h:_) = Just h

