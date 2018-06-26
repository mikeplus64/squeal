{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleInstances
  , FunctionalDependencies
  , KindSignatures
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeFamilies
  , TypeOperators
#-}

module Squeal.PostgreSQL.Type where

import Data.Kind
-- import Generics.SOP
-- import GHC.TypeLits

import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Schema

type PG (hask :: Type) = PGTypeFrom '[] hask

class IsoPG (schema :: SchemaType) (hask :: Type) where
  type family PGTypeFrom schema hask :: PGType
  pg :: TypeExpression schema (PGTypeFrom schema hask)
  toParam :: hask -> Encoding.Encoding
  fromValue :: Decoding.Value hask

instance IsoPG '[] Bool where
  type PGTypeFrom '[] Bool = 'PGbool
  pg = UnsafeTypeExpression "bool"
  toParam = Encoding.bool
  fromValue = Decoding.bool

instance IsoPG schema hask
  => IsoPG (schemum ': schema) hask where
    type PGTypeFrom (schemum ': schema) hask = PGTypeFrom schema hask
    pg = UnsafeTypeExpression . renderTypeExpression $ pg @schema @hask
    toParam = toParam @schema @hask
    fromValue = fromValue @schema @hask
  