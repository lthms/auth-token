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

mkPostTokenGetHandler :: (MonadIO m, MonadError ServantErr m, AuthentMonad id auth m)
                      => (a -> m id)
                      -> (AuthError -> ServantErr)
                      -> a
                      -> m AccessGrant
mkPostTokenGetHandler getId authErrH req = do
    id <- getId req
    auth <- authenticator

    mRes <- liftIO $ getFreshTokens auth id

    case mRes of Right acc -> return acc
                 Left err -> throwError $ authErrH err

postTokenRefreshHandler :: (MonadIO m, MonadError ServantErr m, AuthentMonad id auth m)
                        => (AuthError -> ServantErr)
                        -> PostTokenRefreshReq
                        -> m AccessGrant
postTokenRefreshHandler authErrH (PostTokenRefreshReq tok) = do
    auth <- authenticator
    mRes <- liftIO $ refreshTokens auth tok

    case mRes of Right acc -> return acc
                 Left err -> throwError $ authErrH err

mkAuthServer :: (MonadIO m, MonadError ServantErr m, AuthentMonad id auth m)
             => (a -> m id)
             -> (AuthError -> ServantErr)
             -> ServerT (AuthentApi a) m
mkAuthServer h err = (mkPostTokenGetHandler h err) :<|> postTokenRefreshHandler err
