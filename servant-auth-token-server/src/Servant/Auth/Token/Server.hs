{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Servant.Auth.Token.Server where

import           Control.Monad.Except
import           Auth.Token hiding (newIdentity)
import qualified Auth.Token as A (newIdentity)
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Token
import           Data.Token.Aeson
import           GHC.Generics
import           Servant
import           Servant.Auth.Token.Api

class (Monad m, Authenticator id auth) => AuthentMonad id auth m | m -> auth where
    authenticator :: m auth

newIdentityHandler :: (MonadIO m, MonadError ServantErr m, AuthentMonad id auth m)
                   => m id
newIdentityHandler = do
    auth <- authenticator
    id <- liftIO $ A.newIdentity auth
    return id

mkPostTokenGetHandler :: (MonadError ServantErr m, AuthentMonad id auth m)
                      => (a -> m id)
                      -> a
                      -> m AccessGrant
mkPostTokenGetHandler _ _ = throwError err401

postTokenRefreshHandler :: (MonadError ServantErr m, AuthentMonad id auth m)
                        => PostTokenRefreshReq
                        -> m AccessGrant
postTokenRefreshHandler (PostTokenRefreshReq tok) = throwError err401

mkAuthServer :: (MonadError ServantErr m, AuthentMonad id auth m)
             => (a -> m id)
             -> ServerT (AuthentApi a) m
mkAuthServer h = (mkPostTokenGetHandler h) :<|> postTokenRefreshHandler
