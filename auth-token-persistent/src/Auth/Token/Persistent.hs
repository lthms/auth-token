{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Auth.Token.Persistent where

import           Auth.Token
import qualified Auth.Token.Persistent.Backend as Bd
import           Data.Time
import           Data.Token
import           Data.Token.Persistent
import           Database.Persist.Sql

type Identity = Bd.IdentityId

bdAccessGrantToAccessGrant :: Bd.AccessGrant
                           -> AccessGrant
bdAccessGrantToAccessGrant (Bd.AccessGrant _ acc accE ref refE) =
    AccessGrant (EphemeralToken accE acc)
                (EphemeralToken refE ref)

instance Authenticator Identity ConnectionPool where
    getFreshTokens pool id = do
      exist <- runSqlPool (Bd.idExist id) pool
      dis   <- runSqlPool (Bd.isDisabled id) pool

      if exist
      then return $ Left UnknownIdErr
      else if dis
           then return $ Left DisabledErr
           else do ag <- runSqlPool (Bd.regenTokens id
                                                    60
                                                    6
                                                    3600
                                                    12)
                                    pool

                   return $ Right (bdAccessGrantToAccessGrant ag)

    refreshTokens pool ref = do
      mId <- runSqlPool (Bd.getIdentityByRefresh ref) pool

      case mId of Nothing -> return $ Left InvalidTokenErr
                  Just id -> do ag <- runSqlPool (Bd.regenTokens id
                                                                 60
                                                                 6
                                                                 3600
                                                                 12)
                                                 pool

                                return $ Right (bdAccessGrantToAccessGrant ag)

    disableUntil _ _ _ = return $ Right ()

    newIdentity pool = runSqlPool (Bd.newIdentity) pool

    getIdentity pool tok = do
      mId <- runSqlPool (Bd.getIdentityByAccess tok) pool

      case mId of Nothing -> return $ Left InvalidTokenErr
                  Just id -> return $ Right id

    removeTokens pool tok = do
      mId <- getIdentity pool tok

      case mId of Right id -> do runSqlPool (deleteBy $ Bd.UniqueIdentity id)
                                           pool

                                 return $ Right ()
                  Left err -> return $ Left err

    initAuthenticator pool = runSqlPool (runMigration Bd.migrateAuth) pool
