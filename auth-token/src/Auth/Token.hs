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
  ) where

import           Data.Time
import           Data.Token
import           GHC.TypeLits

data EphemeralToken (level :: Symbol) = EphemeralToken UTCTime (Token level)

hasExpired :: EphemeralToken level -> IO Bool
hasExpired (EphemeralToken exp _) = do now <- getCurrentTime
                                       return (now <= exp)

data AccessGrant = AccessGrant (EphemeralToken "access") (EphemeralToken "refresh")


tokenValue :: EphemeralToken level -> Token level
tokenValue (EphemeralToken _ tok) = tok

data AuthError =
    DisabledErr
  | InvalidTokenErr
  | UnknownIdErr

type AuthRes = Either AuthError

class Authenticator id auth | auth -> id where
    initAuthenticator :: auth -> IO ()
    getFreshTokens    :: auth -> id -> IO (AuthRes AccessGrant)
    refreshTokens     :: auth -> Token "refresh" -> IO (AuthRes AccessGrant)
    removeTokens      :: auth -> Token "access" -> IO (AuthRes ())
    getIdentity       :: auth -> Token "access" -> IO (AuthRes id)
    disableUntil      :: auth -> id -> Maybe UTCTime -> IO (AuthRes ())
