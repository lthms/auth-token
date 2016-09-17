{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DeriveGeneric          #-}

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
import           Data.Token.Aeson
import           GHC.Generics
import           Data.Aeson

data EphemeralToken level = EphemeralToken { expire :: UTCTime
                                           , value :: (Token level)
                                           }
  deriving (Generic)

instance FromJSON (EphemeralToken level)
instance ToJSON (EphemeralToken level)

hasExpired :: EphemeralToken level -> IO Bool
hasExpired (EphemeralToken exp _) = do now <- getCurrentTime
                                       return (now <= exp)

data Access = Access
  deriving (Generic)
data Refresh = Refresh
  deriving (Generic)

type AccessToken = Token Access
type RefreshToken = Token Refresh

data AccessGrant = AccessGrant { access :: (EphemeralToken Access)
                               , refresh :: (EphemeralToken Refresh)
                               }
  deriving (Generic)

instance FromJSON AccessGrant
instance ToJSON AccessGrant

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
