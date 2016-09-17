{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Auth.Token.Persistent.Backend where

import           Auth.Token                   (AccessToken, RefreshToken)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Maybe
import           Data.Time
import           Data.Token
import           Data.Token.Persistent
import           Data.Token.Random
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.Sql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAuth"]
  $(persistFileWith lowerCaseSettings "model.persistent")

newIdentity :: (MonadBaseControl IO m, MonadIO m, Monad m)
            => SqlPersistT m IdentityId
newIdentity = do now <- liftIO $ getCurrentTime
                 insert $ Identity now now

isDisabled :: (MonadBaseControl IO m, MonadIO m, Monad m)
           => IdentityId
           -> SqlPersistT m Bool
isDisabled id = do
    maybeDis <- getBy $ UniqueId id
    case maybeDis of
      Nothing                                 -> return False
      Just (Entity _ (Disabled _ (Just til))) -> do now <- liftIO $ getCurrentTime
                                                    return $ now < til
      _                                       -> return True

idExist :: (MonadBaseControl IO m, MonadIO m, Monad m)
        => IdentityId
        -> SqlPersistT m Bool
idExist id = do maybeId <- get id
                return $ isJust maybeId

getIdentityByAccess :: (MonadBaseControl IO m, MonadIO m, Monad m)
                    => AccessToken
                    -> SqlPersistT m (Maybe IdentityId)
getIdentityByAccess token =
    do maybeToken <- getBy $ UniqueAccess token

       case maybeToken of
         Nothing             -> return Nothing
         Just (Entity _ val) -> do now <- liftIO $ getCurrentTime
                                   if now <= accessGrantAccessExpire val
                                   then do dis <- isDisabled (accessGrantIdentity val)
                                           if not dis
                                             then return . Just $ accessGrantIdentity val
                                             else return Nothing
                                   else return Nothing

getIdentityByRefresh :: (MonadBaseControl IO m, MonadIO m, Monad m)
                    => RefreshToken
                    -> SqlPersistT m (Maybe IdentityId)
getIdentityByRefresh token =
    do maybeToken <- getBy $ UniqueRefresh token

       case maybeToken of
         Nothing             -> return Nothing
         Just (Entity _ val) -> do now <- liftIO $ getCurrentTime
                                   if now <= accessGrantRefreshExpire val
                                   then do dis <- isDisabled (accessGrantIdentity val)
                                           if not dis
                                             then return . Just $ accessGrantIdentity val
                                             else return Nothing
                                   else return Nothing

regenTokens :: (MonadBaseControl IO m, MonadIO m, Monad m)
            => IdentityId
            -> NominalDiffTime -- access token expire in
            -> ByteLength -- access token size
            -> NominalDiffTime -- refresh token expire in
            -> ByteLength -- refresh token size
            -> SqlPersistT m AccessGrant
regenTokens id accessExpire accessSize refreshExpire refreshSize =
    do -- delete old tokens, if any
       deleteBy $ UniqueIdentity id

       -- generate new pair of tokens
       accessTok  <- liftIO $ genRandomToken accessSize
       refreshTok <- liftIO $ genRandomToken refreshSize
       now        <- liftIO $ getCurrentTime

       let tok = AccessGrant id
                             accessTok
                             (addUTCTime accessExpire now)
                             refreshTok
                             (addUTCTime refreshExpire now)

       -- insert new tokens, return nothing if access or
       -- refresh are already in use
       chk <- insertUnique tok

       case chk of Nothing -> regenTokens id accessExpire accessSize refreshExpire refreshSize
                   _ -> return tok
