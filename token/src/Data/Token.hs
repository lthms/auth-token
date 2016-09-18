{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Data.Token
  ( -- * Type
    Token

  -- * Token Creation
  , toText
  , fromText

    -- * Unsafe
  , unsafeTokenCast
  ) where

import           Data.String (IsString)
import           Data.Text (Text)
import           GHC.Generics (Generic)

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
newtype Token scope = Token { unToken :: Text }
  deriving (IsString, Eq, Show, Generic)

-- | Create a new token from a 'ByteString'.
fromText :: Text
         -> Token scope
fromText = Token

-- | Retreive the underlying 'ByteString' of a given token.
toText :: Token scope
       -> Text
toText = unToken


-- | __This should not be used unless you know what to do.__
--
-- You can cast a Token with a given scope through a Token with another
-- scope.
unsafeTokenCast :: Token scope -> Token scope'
unsafeTokenCast (Token str) = Token str
