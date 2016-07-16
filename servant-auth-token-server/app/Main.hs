{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Auth.Token
import Auth.Token.Persistent
import Control.Monad.Except
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader
import Database.Persist.Sqlite
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Token.Api
import Servant.Auth.Token.Server

type AuthM = ReaderT ConnectionPool (ExceptT ServantErr IO)

instance AuthentMonad Identity ConnectionPool AuthM where
    authenticator = ask

type MyApi = AuthentApi Int
        :<|> "new" :> PostCreated '[JSON] Identity

newHandler :: AuthM Identity
newHandler = do
    auth <- authenticator
    id <- liftIO $ newIdentity auth

    return id

authToExcept :: ConnectionPool
             -> AuthM
             :~> ExceptT ServantErr IO
authToExcept pool = Nat $ \x -> runReaderT x pool

authServe :: ConnectionPool
          -> ServerT MyApi AuthM
          -> Server MyApi
authServe pool = enter $ authToExcept pool

server :: ServerT MyApi AuthM
server = (mkAuthServer (\ _ -> throwError err401)) :<|> newHandler

myApi :: Proxy MyApi
myApi = Proxy

authd :: ConnectionPool
      -> Application
authd pool = logStdout $ serve myApi (authServe pool server)

main :: IO ()
main = do pool <- runNoLoggingT $ createSqlitePool "db.sqlite" 10
          initAuthenticator pool

          run 8080 (authd pool)
