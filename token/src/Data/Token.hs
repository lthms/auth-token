{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Token
  ( -- * Type
    Token

  -- * Token Creation
  , toByteString
  , fromByteString

    -- * Unsafe
  , unsafeTokenCast
  ) where

import           Data.ByteString (ByteString)
import           Data.String
import           GHC.TypeLits    (Symbol)

-- | A 'Token' is 'ByteString' subtype with an explicit scope in the form
-- of a string. This allows to define two ByteString variables which cannot
-- be mixed by mistake.
--
-- >  private :: Token "private"
-- >  private = "a private shared secret"
-- >
-- >  public :: Token "public"
-- >  public = "public message for everyone"
-- >
-- >  public == private -- type system error
newtype Token (scope :: Symbol) = Token { unToken :: ByteString }
  deriving (IsString, Eq, Show)

-- | Create a new token from a 'ByteString'.
fromByteString :: ByteString
               -> Token scope
fromByteString = Token

-- | Retreive the underlying 'ByteString' of a given token.
toByteString :: Token scope
             -> ByteString
toByteString = unToken


-- | __This should not be used unless you know what to do.__
--
-- You can cast a Token with a given scope through a Token with another
-- scope.
unsafeTokenCast :: Token scope -> Token scope'
unsafeTokenCast (Token str) = Token str
