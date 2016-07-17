module Data.Token.Aeson where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Token

instance ToJSON (Token scope) where
    toJSON = String . decodeUtf8 . toByteString

instance FromJSON (Token scope) where
    parseJSON (String val) = return . fromByteString . encodeUtf8 $ val
    parseJSON _ = mzero
