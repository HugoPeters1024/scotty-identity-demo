{-# LANGUAGE OverloadedStrings #-}

module Auth (
  useAuth,
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
import Web.Scotty.Cookie (setSimpleCookie)
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived, vault, queryString)
import qualified Data.Vault.Lazy as V
import Data.Aeson (FromJSON, ToJSON, encode, decode, parseJSON, withObject, (.:))
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.String (IsString)
import Data.UUID

data LoginRequest = LoginRequest {
    l_userName :: T.Text
  , l_password :: T.Text
} deriving (Show)

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \v -> LoginRequest
    <$> v .: "username"
    <*> v .: "password"


useAuth :: (Show c, Show b, IsSession b, Show a, IsUser a) => 
           (Request -> IO (Maybe c))    -- readSession
        -> (UUID -> IO (Maybe b))    -- getSession
        -> (b -> IO Bool)         -- verifySession
        -> (b -> IO (Maybe a))    -- sessionToUser
        -> (Username -> IO (Maybe a)) -- retrieveUser
        -> (a -> IO b)                 -- createSession
        -> IO (Middleware, (a -> ActionM ()) -> ActionM (), ActionM ())
useAuth readSession getSession verifySession sessionToUser retrieveUser makeSession = do
  key <- V.newKey :: IO (V.Key a)
  return (middleware key, makeAuth key, doLogin retrieveUser makeSession)
  where
    middleware key app req res = do
        --mSessionKey <- readSession req
        let query = queryString req :: [(B.ByteString, Maybe B.ByteString)]
            mToken = join $ lookup "token" query :: Maybe B.ByteString
            mSessionKey = mToken >>= fromString . show :: Maybe UUID
        case mSessionKey of
              Nothing -> do
                    putStrLn "malformed uuid"
                    app req res
              Just session_key -> do 
                    print session_key
                    mSession <- getSession session_key
                    case mSession of
                          Nothing -> do
                                putStrLn "session not found"
                                app req res
                          Just session -> do
                                print session
                                mUser <- sessionToUser session
                                case mUser of 
                                      Nothing -> do
                                            putStrLn "user from session not found (db violation??)"
                                            app req res
                                      Just user -> do
                                            print user
                                            let vault' = V.insert key user (vault req)
                                                req'   = req { vault = vault' }
                                            app req' res

makeAuth :: (V.Key a) -> (a -> ActionM ()) -> ActionM ()
makeAuth key authRoute = do
    req <- request
    case V.lookup key (vault req) of
        Nothing -> text "not authenticated"
        Just u  -> authRoute u

doLogin :: (IsUser a, IsSession b) => (Username -> IO (Maybe a)) -> (a -> IO b) -> ActionM ()
doLogin retrieveUser makeSession = do
    username <- param "username"
    password <- param "password"
    mUser <- liftAndCatchIO $ retrieveUser username
    case mUser of
        Nothing -> text "unkown user"
        Just u  -> do
            case password == getPassword u of
                  False -> text "wrong password"
                  True  -> do
                        s <- liftAndCatchIO $ makeSession u
                        setLoginCookie s
                        redirect "/user"


parseLogin :: LT.Text -> Maybe LoginRequest
parseLogin t = case LT.splitOn ":" t of
      [a, b] -> Just $ LoginRequest (LT.toStrict a) (LT.toStrict b)
      _      -> Nothing

setLoginCookie :: IsSession a => a -> ActionM ()
setLoginCookie session = setSimpleCookie "Authorization" (getIdentifier session)

type Username = T.Text 
type Password = T.Text 

class IsUser a where
  getUsername :: a -> Username
  getPassword :: a -> Password

class IsSession a where
  getIdentifier :: a -> T.Text

test :: Maybe LoginRequest
test = decode "{\"username\": \"lol\", \"password\": \"2xlol\"}"
