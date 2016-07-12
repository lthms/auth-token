module Data.Token.Random
  ( ByteLength
  , genRandomToken
  ) where

import           Crypto.Random
import           Crypto.Types
import           Data.Token

genRandomToken :: ByteLength -> IO (Token level)
genRandomToken len = do
    g <- newGenIO :: IO SystemRandom
    let try = genBytes len g

    case try of Left _         -> fail "should not happen"
                Right (str, _) -> return $ fromByteString str
