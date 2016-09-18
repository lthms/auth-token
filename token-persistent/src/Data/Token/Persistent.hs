{-# LANGUAGE OverloadedStrings #-}

module Data.Token.Persistent where

import           Data.Token (Token, fromText, toText)
import           Database.Persist.Class
import           Database.Persist.Sql
import           Database.Persist.Types (SqlType(..))

instance PersistField (Token level) where
  toPersistValue = PersistText . toText

  fromPersistValue (PersistText bs) = Right $ fromText bs
  fromPersistValue _                = Left "Token must be converted from Text"

instance PersistFieldSql (Token level) where
  sqlType _ = SqlString
