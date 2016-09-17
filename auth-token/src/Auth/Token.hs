{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Auth.Token
  ( EphemeralToken (..)
  , hasExpired
  , tokenValue
  , AccessGrant (..)
  , Authenticator (..)
  , AuthError (..)
  , AuthRes
  , Access
  , Refresh
  , AccessToken
  , RefreshToken
  ) where

import           Data.Time
import           Data.Token

data EphemeralToken level = EphemeralToken UTCTime (Token level)

hasExpired :: EphemeralToken level -> IO Bool
hasExpired (EphemeralToken exp _) = do now <- getCurrentTime
                                       return (now <= exp)

data Access
data Refresh

type AccessToken = Token Access
type RefreshToken = Token Refresh

data AccessGrant = AccessGrant (EphemeralToken Access) (EphemeralToken Refresh)

tokenValue :: EphemeralToken level -> Token level
tokenValue (EphemeralToken _ tok) = tok

data AuthError =
    DisabledErr
  | InvalidTokenErr
  | UnknownIdErr

type AuthRes = Either AuthError

class Authenticator auth where
    type family Id auth :: *
    initAuthenticator :: auth -> IO ()
    newIdentity       :: auth -> IO (Id auth)
    getFreshTokens    :: auth -> (Id auth) -> IO (AuthRes AccessGrant)
    refreshTokens     :: auth -> Token Refresh -> IO (AuthRes AccessGrant)
    removeTokens      :: auth -> Token Access -> IO (AuthRes ())
    getIdentity       :: auth -> Token Access -> IO (AuthRes (Id auth))
    disableUntil      :: auth -> (Id auth) -> Maybe UTCTime -> IO (AuthRes ())
