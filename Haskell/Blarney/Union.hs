{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Blarney.Union
Description : Union types
Copyright   : (c) Matthew Naylor, 2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module defines type-indexed union types.

-}
module Blarney.Union where

-- Blarney imports
import Blarney
import Blarney.Option
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Utils
import Blarney.TypeFamilies

-- GHC imports
import GHC.Types
import Data.Type.Bool
import Data.Type.Equality

-- | Union type
data Union (ts :: [Type]) =
  Union {
    memberIdx :: BV
  , memberVal :: BV
  }

-- | Bottom value, used internally
bottom :: a
bottom = error "Blarney.Union.bottom"

-- | Determine the index of a member in a union type
class UnionMember t ts where
  unionIndex :: t -> Union ts -> Int

instance {-# OVERLAPPING #-} UnionMember t (t ': ts) where
  unionIndex _ _ = 0

instance UnionMember t ts => UnionMember t (u ': ts) where
  unionIndex _ _ = 1 + unionIndex (bottom :: t) (bottom :: Union ts)

-- | Determine the number of members and union width
class UnionInfo ts where
  unionCount :: Union ts -> Int
  unionMaxWidth :: Union ts -> Int

instance UnionInfo '[] where
  unionCount _ = 0
  unionMaxWidth _ = 0

instance (Bits t, UnionInfo ts) => UnionInfo (t ': ts) where
  unionCount _ = 1 + unionCount (bottom :: Union ts)
  unionMaxWidth _ = max (sizeOf (bottom :: t))
                        (unionMaxWidth (bottom :: Union ts))

-- | Bits instance for union types
instance UnionInfo ts => Bits (Union ts) where
  type SizeOf (Union ts) = 1 + Log2 (Length ts) + MaxSizeOf ts
  sizeOf u = 1 + log2 (unionCount u) + unionMaxWidth u
  pack u = FromBV $ concatBV u.memberIdx u.memberVal
  unpack inp =
    Union {
      memberIdx = selectBV (msbIdx-1, maxMemberWidth) (toBV inp)
    , memberVal = selectBV (maxMemberWidth-1, 0) (toBV inp)
    }
    where
      numMembers = unionCount (bottom :: Union ts)
      maxMemberWidth = unionMaxWidth (bottom :: Union ts)
      msbIdx = 1 + log2 numMembers + maxMemberWidth
  nameBits nm u =
    Union {
      memberIdx = addBVNameHint u.memberVal (NmRoot 0 ("uid" ++ nm))
    , memberVal = addBVNameHint u.memberVal (NmRoot 0 ("uval" ++ nm))
    }

-- | Create an instance of a union type
toUnion :: forall ts t. (Bits t, UnionInfo ts, UnionMember t ts)
        => t -> Union ts
toUnion x =
  Union {
    memberIdx = constBV (1 + log2 numMembers) (fromIntegral idx)
  , memberVal = zeroExtendBV maxMemberWidth (toBV (pack x))
  }
  where
    idx = unionIndex x (bottom :: Union ts)
    numMembers = unionCount (bottom :: Union ts)
    maxMemberWidth = unionMaxWidth (bottom :: Union ts)

-- | Extract a member from a union type
fromUnion :: forall t ts. (Bits t, UnionInfo ts, UnionMember t ts)
          => Union ts -> Option t
fromUnion u =
  Option {
    valid = FromBV $ equalBV u.memberIdx
                             (constBV idxWidth (fromIntegral idx))
  , val = unpack $ FromBV $ selectBV (sizeOf (bottom :: t) - 1, 0) u.memberVal
  }
  where
    idx = unionIndex (bottom :: t) (bottom :: Union ts)
    idxWidth = 1 + log2 numMembers
    numMembers = unionCount (bottom :: Union ts)
