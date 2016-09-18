module Data.Token.Random
  ( ByteLength
  , genRandomToken
  ) where

import           Crypto.Random (newGenIO, SystemRandom, genBytes)
import           Crypto.Types (ByteLength)
import           Data.ByteString.Base64 as B64
import           Data.Text (pack)
import           Data.Token (Token, fromText)
import           Data.Text.Encoding (decodeUtf8)

genRandomToken :: ByteLength -> IO (Token level)
genRandomToken len = do
    g <- newGenIO :: IO SystemRandom
    let try = genBytes len g

    case try of Left _         -> fail "should not happen"
                Right (str, _) -> return . fromText . decodeUtf8 . B64.encode $ str
