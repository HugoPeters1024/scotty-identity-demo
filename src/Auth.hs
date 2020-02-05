{-# LANGUAGE OverloadedStrings #-}

module Auth (
  useAuth,
  removeLoginCookie,
  IsUser,
  getUsername,
  getPassword,
  IsSession,
  getIdentifier,
  Username,
  Password) where

import Control.Monad (liftM, join)
import Data.Maybe (fromJust, fromMaybe)
import Web.Scotty (ActionM, request, text, redirect, param, liftAndCatchIO, header)
import Web.Scotty.Cookie (setSimpleCookie, getCookie, deleteCookie)
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived, vault, queryString)
import qualified Data.Vault.Lazy as V
import Data.Aeson (FromJSON, ToJSON, encode, decode, parseJSON, withObject, (.:))
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.String (IsString)
import Data.UUID

useAuth :: (Show b, IsSession b, Show a, IsUser a) => 
           (UUID -> IO (Maybe b))      -- getSession
        -> (b -> IO Bool)              -- verifySession
        -> (b -> IO (Maybe a))         -- sessionToUser
        -> (Username -> IO (Maybe a))  -- retrieveUser
        -> (a -> IO b)                 -- createSession
        -> IO ((a -> ActionM ()) -> ActionM (), ActionM ())
useAuth getSession verifySession sessionToUser retrieveUser makeSession = do
  return (makeAuth, doLogin retrieveUser makeSession)
  where
    makeAuth needUser = do
        strToken <- liftM (fromMaybe "") $ getCookie "Authorization" :: ActionM T.Text
        case fromText strToken of
            Nothing -> redirect "/login"
            Just token -> do
                  mSession <- liftAndCatchIO $ getSession token
                  case mSession of
                        Nothing -> redirect "/login"
                        Just session -> do
                              mUser <- liftAndCatchIO $ sessionToUser session
                              case mUser of 
                                    Nothing -> text "user from session not found (db violation??)"
                                    Just user -> needUser user

doLogin :: (IsUser a, IsSession b) => (Username -> IO (Maybe a)) -> (a -> IO b) -> ActionM ()
doLogin retrieveUser makeSession = do
    username <- param "username"
    password <- param "password"
    mUser <- liftAndCatchIO $ retrieveUser username
    case mUser of
        Nothing -> redirect "/login"
        Just u  -> do
            case password == getPassword u of
                  False -> redirect "/login"
                  True  -> do
                        s <- liftAndCatchIO $ makeSession u
                        setLoginCookie s
                        redirect "/user"


setLoginCookie :: IsSession a => a -> ActionM ()
setLoginCookie session = setSimpleCookie "Authorization" ((toText . getIdentifier) session)

removeLoginCookie :: ActionM ()
removeLoginCookie = do
    deleteCookie "Authorization"
    redirect "/login"

type Username = T.Text 
type Password = T.Text 

class IsUser a where
  getUsername :: a -> Username
  getPassword :: a -> Password

class IsSession a where
  getIdentifier :: a -> UUID
