{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE FlexibleInstances      #-}

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
import           Data.Text.Encoding (encodeUtf8)
import           Network.Wai
import           Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import           Data.ByteString

type instance AuthServerData (AuthProtect auth) = Id auth

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

authTokenHandler :: (Authenticator auth)
                 => auth
                 -> (AuthError -> ServantErr)
                 -> AuthHandler Request (Id auth)
authTokenHandler auth errH =
    let handler req = case lookup "auth-token" (requestHeaders req) of
                          Nothing  -> throwError $ err401 { errBody = "Missing auth-token header" }
                          Just str -> let tok :: Token "access"
                                          tok = fromByteString str
                                      in do mId <- liftIO $ getIdentity auth tok
                                            case mId of Right id -> return id
                                                        Left err -> throwError $ errH err
    in mkAuthHandler handler

authTokenContext :: (Authenticator auth)
                 => auth
                 -> (AuthError -> ServantErr)
                 -> Context (AuthHandler Request (Id auth) ': '[])
authTokenContext auth errH = authTokenHandler auth errH :. EmptyContext

mkAuthServer :: (MonadIO m, MonadError ServantErr m, AuthentMonad auth m)
             => (a -> m (Id auth))
             -> (AuthError -> ServantErr)
             -> ServerT (AuthentApi a) m
mkAuthServer h err = (postTokenGetHandler h err) :<|> postTokenRefreshHandler err
