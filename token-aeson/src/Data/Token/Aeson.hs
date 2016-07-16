module Data.Token.Aeson where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Token
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.ByteString.Base64 as B64
import           Control.Monad

instance ToJSON (Token scope) where
    toJSON = String . decodeUtf8 . B64.encode . toByteString

instance FromJSON (Token scope) where
    parseJSON (String val) =
      case B64.decode $ encodeUtf8 val of
        Left _   -> mzero
        Right bs -> return $ fromByteString bs
    parseJSON _ = mzero
