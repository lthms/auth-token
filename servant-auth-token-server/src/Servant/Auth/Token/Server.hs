{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Servant.Auth.Token.Server where

import           Auth.Token                       hiding (newIdentity)
import qualified Auth.Token                       as A (newIdentity)
import           Control.Monad                    (mzero)
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString
import           Data.Text.Encoding               (decodeUtf8)
import           Data.Token
import           Data.Token.Aeson
import           GHC.Generics
import           Network.Wai
import           Servant
import           Servant.Auth.Token.Api
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)

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
                 -> (Id auth -> IO (AuthServerData TokenProtect))
                 -> (AuthError -> ServantErr)
                 -> AuthHandler Request (AuthServerData TokenProtect)
authTokenHandler auth getData errH =
    let handler req = case lookup "auth-token" (requestHeaders req) of
                          Nothing  -> throwError $ err401 { errBody = "Missing auth-token header" }
                          Just str -> let tok :: AccessToken
                                          tok = fromText . decodeUtf8 $ str
                                      in do mId <- liftIO $ getIdentity auth tok
                                            case mId of Right id -> do liftIO $ getData id
                                                        Left err -> throwError $ errH err
    in mkAuthHandler handler

authTokenContext :: (Authenticator auth)
                 => auth
                 -> (Id auth -> IO (AuthServerData TokenProtect))
                 -> (AuthError -> ServantErr)
                 -> Context (AuthHandler Request (AuthServerData TokenProtect) ': '[])
authTokenContext auth getData errH = authTokenHandler auth getData errH :. EmptyContext

mkAuthServer :: (MonadIO m, MonadError ServantErr m, AuthentMonad auth m)
             => (a -> m (Id auth))
             -> (AuthServerData TokenProtect -> m b)
             -> (AuthError -> ServantErr)
             -> ServerT (AuthentApi a b) m
mkAuthServer h whoami err = ((postTokenGetHandler h err) :<|> postTokenRefreshHandler err) :<|> whoami
