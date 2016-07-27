{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import           Auth.Token
import           Auth.Token.Persistent
import           Control.Monad.Except
import           Control.Monad.Logger                 (runNoLoggingT)
import           Control.Monad.Reader
import           Database.Persist.Sqlite
import           GHC.Int
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           Servant.Auth.Token.Api
import           Servant.Auth.Token.Server
import           Servant.Server.Experimental.Auth     (AuthServerData)

type instance AuthServerData TokenProtect = Int64

type AuthM = ReaderT ConnectionPool (ExceptT ServantErr IO)

instance AuthentMonad ConnectionPool AuthM where
    authenticator = ask

type MyApi = AuthentApi Int64 Int64
        :<|> "new" :> PostCreated '[JSON] Int64

whoamiHandler :: Int64 -> AuthM Int64
whoamiHandler = return

newHandler :: AuthM Int64
newHandler = do
    auth <- authenticator
    id <- liftIO $ newIdentity auth

    return $ fromSqlKey id

authToExcept :: ConnectionPool
             -> AuthM
             :~> ExceptT ServantErr IO
authToExcept pool = Nat $ \x -> runReaderT x pool

authServe :: ConnectionPool
          -> ServerT MyApi AuthM
          -> Server MyApi
authServe pool = enter $ authToExcept pool

server :: ServerT MyApi AuthM
server = (mkAuthServer getIdentityHandler whoamiHandler authentErrorHandler)
    :<|> newHandler

getIdentityHandler :: Int64 -> AuthM Identity
getIdentityHandler id = return $ toSqlKey id

authentErrorHandler :: AuthError -> ServantErr
authentErrorHandler DisabledErr = err403 { errBody = "This account has been disabled" }
authentErrorHandler InvalidTokenErr = err403 { errBody = "Invalid token" }
authentErrorHandler UnknownIdErr = err403 { errBody = "Unknown user" }

myApi :: Proxy MyApi
myApi = Proxy

authd :: ConnectionPool
      -> Application
authd pool = logStdout $ serveWithContext myApi
                                          (authTokenContext pool (return . fromSqlKey) authentErrorHandler)
                                          (authServe pool server)

main :: IO ()
main = do pool <- runNoLoggingT $ createSqlitePool "db.sqlite" 10
          initAuthenticator pool

          run 8080 (authd pool)
