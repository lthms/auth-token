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

class Authenticator auth where
    type family Id auth :: *
    initAuthenticator :: auth -> IO ()
    newIdentity       :: auth -> IO (Id auth)
    getFreshTokens    :: auth -> (Id auth) -> IO (AuthRes AccessGrant)
    refreshTokens     :: auth -> Token "refresh" -> IO (AuthRes AccessGrant)
    removeTokens      :: auth -> Token "access" -> IO (AuthRes ())
    getIdentity       :: auth -> Token "access" -> IO (AuthRes (Id auth))
    disableUntil      :: auth -> (Id auth) -> Maybe UTCTime -> IO (AuthRes ())
