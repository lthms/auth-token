{-# LANGUAGE OverloadedStrings #-}

module Data.Token.Persistent where

import           Data.Token
import           Database.Persist.Class
import           Database.Persist.Sql
import           Database.Persist.Types

instance PersistField (Token level) where
  toPersistValue = PersistByteString . toByteString

  fromPersistValue (PersistByteString bs) = Right $ fromByteString bs
  fromPersistValue _                      = Left "Token must be converted from ByteString"

instance PersistFieldSql (Token level) where
  sqlType _ = SqlBlob
