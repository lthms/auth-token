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

class (Monad m, Authenticator auth) => AuthentMonad auth m | m -> auth where
    authenticator :: m auth

newIdentityHandler :: (MonadIO m, MonadError ServantErr m, AuthentMonad auth m)
                   => m (Id auth)
newIdentityHandler = do
    auth <- authenticator
    id <- liftIO $ A.newIdentity auth
    return id

postTokenGetHandler :: (MonadIO m, MonadError ServantErr m, AuthentMonad auth m)
                    => (a -> m (Id auth))
                    -> (AuthError -> ServantErr)
                    -> a
                    -> m AccessGrant
postTokenGetHandler getId authErrH req = do
    id <- getId req
    auth <- authenticator

    mRes <- liftIO $ getFreshTokens auth id

    case mRes of Right acc -> return acc
                 Left err -> throwError $ authErrH err

postTokenRefreshHandler :: (MonadIO m, MonadError ServantErr m, AuthentMonad auth m)
                        => (AuthError -> ServantErr)
                        -> PostTokenRefreshReq
                        -> m AccessGrant
postTokenRefreshHandler authErrH (PostTokenRefreshReq tok) = do
    auth <- authenticator
    mRes <- liftIO $ refreshTokens auth tok

    case mRes of Right acc -> return acc
                 Left err -> throwError $ authErrH err

mkAuthServer :: (MonadIO m, MonadError ServantErr m, AuthentMonad auth m)
             => (a -> m (Id auth))
             -> (AuthError -> ServantErr)
             -> ServerT (AuthentApi a) m
mkAuthServer h err = (postTokenGetHandler h err) :<|> postTokenRefreshHandler err
