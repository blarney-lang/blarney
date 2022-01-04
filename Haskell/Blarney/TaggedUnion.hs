{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

module Blarney.TaggedUnion
  ( TaggedUnion
  , TagName
  , (:::)
  , module GHC.OverloadedLabels
  , hasTag, is
  , tag, untag
  , whenTag
  ) where

import GHC.Types
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Type.Bool
import Data.Type.Equality

import Blarney
import Blarney.Option
import Blarney.Core.BV
import Blarney.TypeFamilies

-- | For specifying the type of a named field of a tagged union
data (a :: Symbol) ::: (b :: Type)

{-| A abstract type for /tagged unions/. Example usage:
    > type Either a b = TaggedUnion ["left" ::: a, "right" ::: b]
-}
data TaggedUnion (fields :: [Type]) =
  TaggedUnion {
    memberIdx :: BV
  , memberVal :: BV
  }

-- Overloaded labels
-- =================

-- | Wrapper for field name symbols
data TagName (sym :: Symbol) = TagName

-- | Tag names are labels
instance sym0 ~ sym1 => IsLabel sym0 (TagName sym1) where
  fromLabel = TagName

-- Type familes
-- ============

-- | Extract the type of a given field
type family GetField name fields where
  GetField name '[] = 
    TypeError (Text "TaggedUnion: tag " :<>:
                 ShowType name :<>: Text " doesn't exist")
  GetField name ((n ::: ty) ': rest) =
    If (name == n) ty (GetField name rest)

-- | Extract the index of a given field
type family GetFieldIdx name fields where
  GetFieldIdx name '[] =
    TypeError (Text "TaggedUnion: tag " :<>:
                 ShowType name :<>: Text " doesn't exist")
  GetFieldIdx name ((n ::: ty) ': rest) =
    If (name == n) 0 (1 + GetFieldIdx name rest)

-- | Determine max field size
type family MaxFieldSize fields where
  MaxFieldSize '[] = 0
  MaxFieldSize ((n ::: ty) ': rest) = Max (SizeOf ty) (MaxFieldSize rest)

-- Type classes
-- ============

-- | Bottom value, used internally
bottom :: a
bottom = error "Blarney.TaggedUnion.bottom"

-- | Determine the number of members and union width
class TaggedUnionInfo fields where
  taggedUnionCount :: TaggedUnion fields -> Int
  taggedUnionMaxWidth :: TaggedUnion fields -> Int

instance TaggedUnionInfo '[] where
  taggedUnionCount _ = 0
  taggedUnionMaxWidth _ = 0

instance (Bits t, TaggedUnionInfo rest) =>
           TaggedUnionInfo ((nm ::: t) ': rest) where
  taggedUnionCount _ = 1 + taggedUnionCount (bottom :: TaggedUnion rest)
  taggedUnionMaxWidth _ =
    max (sizeOf (bottom :: t))
        (taggedUnionMaxWidth (bottom :: TaggedUnion rest))

-- Bits instance
-- =============

-- | Bits instance for tagged unions
instance TaggedUnionInfo fields => Bits (TaggedUnion fields) where
  type SizeOf (TaggedUnion fields) =
    Log2Ceil (Length fields) + MaxFieldSize fields
  sizeOf u = log2ceil (taggedUnionCount u) + taggedUnionMaxWidth u
  pack u = FromBV $ concatBV u.memberIdx u.memberVal
  unpack inp =
    TaggedUnion {
      memberIdx = selectBV (msbIdx-1, maxMemberWidth) (toBV inp)
    , memberVal = selectBV (maxMemberWidth-1, 0) (toBV inp)
    }
    where
      numMembers = taggedUnionCount (bottom :: TaggedUnion fields)
      maxMemberWidth = taggedUnionMaxWidth (bottom :: TaggedUnion fields)
      msbIdx = log2ceil numMembers + maxMemberWidth
  nameBits nm u =
    TaggedUnion {
      memberIdx = addBVNameHint u.memberIdx (NmRoot 0 ("uidx" ++ nm))
    , memberVal = addBVNameHint u.memberVal (NmRoot 0 ("uval" ++ nm))
    }

-- API
-- ===

-- | Does given tagged union have given tag?
hasTag :: forall name fields fieldIdx.
     ( TaggedUnionInfo fields
     , fieldIdx ~ GetFieldIdx name fields
     , KnownNat fieldIdx )
  => TaggedUnion fields -> TagName name -> Bit 1
hasTag u _ = FromBV $ equalBV (constBV w i) u.memberIdx 
  where
    i = fromIntegral (valueOf @fieldIdx)
    w = log2ceil (taggedUnionCount (bottom :: TaggedUnion fields))

-- | Shorthand for hasTag
infix 7 `is`
is :: forall name fields fieldIdx.
     ( TaggedUnionInfo fields
     , fieldIdx ~ GetFieldIdx name fields
     , KnownNat fieldIdx )
  => TaggedUnion fields -> TagName name -> Bit 1
is = hasTag

-- | Construct a value of a tagged union
tag :: forall name fields fieldIdx field.
     ( TaggedUnionInfo fields
     , fieldIdx ~ GetFieldIdx name fields
     , KnownNat fieldIdx
     , field ~ GetField name fields
     , Bits field )
  => TagName name -> field -> TaggedUnion fields
tag _ x = 
  TaggedUnion {
    memberIdx = constBV (log2ceil numMembers) i
  , memberVal = zeroExtendBV maxMemberWidth (toBV (pack x))
  }
  where
    i = fromIntegral (valueOf @fieldIdx)
    numMembers = taggedUnionCount (bottom :: TaggedUnion fields)
    maxMemberWidth = taggedUnionMaxWidth (bottom :: TaggedUnion fields)

-- | Get the value of given field if the tag matches
untag :: forall name fields fieldIdx field.
     ( TaggedUnionInfo fields
     , field ~ GetField name fields
     , fieldIdx ~ GetFieldIdx name fields
     , KnownNat fieldIdx
     , Bits field )
  => TagName name -> Maybe field -> TaggedUnion fields -> field
untag _ defaultVal u =
  case defaultVal of
    Nothing -> val
    Just def -> match ? (val, def)
  where
    i = fromIntegral (valueOf @fieldIdx)
    wi = log2ceil (taggedUnionCount (bottom :: TaggedUnion fields))
    numMembers = taggedUnionCount (bottom :: TaggedUnion fields)
    maxMemberWidth = taggedUnionMaxWidth (bottom :: TaggedUnion fields)
    fieldSize = sizeOf (bottom :: field)
    val = unpack $ FromBV $ selectBV (fieldSize-1, 0) u.memberVal
    match = FromBV $ equalBV (constBV wi i) u.memberIdx

-- | Conditional statement for tagged unions
whenTag ::
     ( TaggedUnionInfo fields
     , KnownNat (GetFieldIdx name fields)
     , field ~ GetField name fields
     , Bits field )
  => TagName name
  -> TaggedUnion fields
  -> (field -> Action a) -> Action a
whenTag tag u f = whenAction (u `hasTag` tag) (f (untag tag Nothing u))
