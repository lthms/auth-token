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

import Control.Monad.Except
import Auth.Token
import Control.Monad (mzero)
import Data.Aeson
import Data.Token
import Data.Token.Aeson
import GHC.Generics
import Servant
import Servant.Auth.Token.Api

class (Monad m, Authenticator id auth) => AuthentMonad id auth m | m -> auth where
    authenticator :: m auth

postTokenRefreshHandler :: (MonadError ServantErr m, AuthentMonad id auth m)
                        => PostTokenRefreshReq
                        -> m AccessGrant
postTokenRefreshHandler (PostTokenRefreshReq tok) = throwError err401

mkAuthServer :: (MonadError ServantErr m, AuthentMonad id auth m)
             => (a -> m AccessGrant)
             -> ServerT (AuthentApi a) m
mkAuthServer h = h :<|> postTokenRefreshHandler
