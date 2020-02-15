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
import Web.Scotty.Identity
import Web.Scotty.Cookie
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Aeson (FromJSON, ToJSON, encode, decode, parseJSON, withObject, (.:))
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived, vault, strictRequestBody, lazyRequestBody)
import Crypto.Hash.SHA256 (hash)
import Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4

import User
import Post
import ErrorRes
import UserPage
import LoginPage
import Session

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
  let (mkAuth, mkAuthM, routeLoginPost, routeLogout) = configureAuth (getSession conn) (sessionToUser conn) (retrieveUser conn) (createSession conn) hash
  scotty 3000 $ do
    get "/user"   $ mkAuth $ routeUser conn
    post "/login" $ routeLoginPost >>= \_ -> redirect "user"
    get "/login"  $ mkAuthM $ routeLogin
    get "/logout" $ routeLogout >>= \_ -> redirect "/login"

getSession :: Connection -> UUID -> IO (Maybe Session)
getSession conn key = headM <$> query conn "select * from sessions where key = ?" [key]

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

routeLogin :: Maybe User -> ActionM ()
routeLogin Nothing = fromHtml loginPage
routeLogin (Just _) = redirect "/user"

routeUser :: Connection -> User -> ActionM ()
routeUser conn user = do
    posts <- liftAndCatchIO $ getUserPosts conn (u_userId user)
    html $ renderHtml $ userPage user posts

--fromHtml :: Html -> ActionM ()
fromHtml page = html $ renderHtml page

getUser :: Connection -> Int -> IO (Maybe User)
getUser conn id = headM <$> query conn "select * from users where user_id = ?" [id]

getPosts :: Connection -> IO (Maybe Post)
getPosts conn = headM <$> query_ conn "select * from posts"

getUserPosts :: Connection -> Int -> IO [Post]
getUserPosts conn id = query conn "select * from posts where user_id = ?" [id]

headM :: [a] -> Maybe a
headM []    = Nothing
headM (h:_) = Just h

