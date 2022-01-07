{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Blarney.TaggedUnion
  ( TaggedUnion
  , TagName
  , (:::)
  , module GHC.OverloadedLabels
  , isTagged, is
  , tag, untag, untagDefault
  , whenTagged
  , IsTaggedUnion(..)
  , HasMember(..)
  ) where

-- GHC imports
import GHC.Types
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy

-- Blarney imports
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

-- | Determine max field size
type family MaxFieldSize fields where
  MaxFieldSize '[] = 0
  MaxFieldSize ((n ::: ty) ': rest) = Max (SizeOf ty) (MaxFieldSize rest)

-- Type classes
-- ============

-- | Bottom value, used internally
bottom :: a
bottom = error "Blarney.TaggedUnion.bottom"

class IsTaggedUnion t where
  getNumMembers :: t -> Int
  getMaxMemberWidth :: t -> Int
  toRaw :: t -> (BV, BV)
  fromRaw :: (BV, BV) -> t

instance IsTaggedUnion (TaggedUnion '[]) where
  getNumMembers _ = 0
  getMaxMemberWidth _ = 0
  toRaw u = (u.memberIdx, u.memberVal)
  fromRaw (idx, val) = TaggedUnion { memberIdx = idx, memberVal = val }

instance (Bits t, IsTaggedUnion (TaggedUnion rest))
      => IsTaggedUnion (TaggedUnion ((tag ::: t) ': rest)) where
  getNumMembers _ = 1 +  getNumMembers (bottom :: TaggedUnion rest)
  getMaxMemberWidth _ =
    max (sizeOf (bottom :: t))
        (getMaxMemberWidth (bottom :: TaggedUnion rest))
  toRaw u = (u.memberIdx, u.memberVal)
  fromRaw (idx, val) = TaggedUnion { memberIdx = idx, memberVal = val }

class IsTaggedUnionMember tag memberTy members | tag members -> memberTy where
  getTagIdx :: TagName tag -> memberTy -> TaggedUnion members -> Integer

instance {-# OVERLAPPING #-}
         IsTaggedUnionMember tag memberTy ((tag ::: memberTy) ': rest) where
  getTagIdx _ _ _ = 0

instance IsTaggedUnionMember tag memberTy rest
      => IsTaggedUnionMember tag memberTy ((tag' ::: memberTy') ': rest) where
  getTagIdx _ _ _ =
    1 + getTagIdx (bottom :: TagName tag)
                       (bottom :: memberTy)
                       (bottom :: TaggedUnion rest)

class IsTaggedUnion unionTy => HasMember tag memberTy unionTy
                                 | tag unionTy -> memberTy where
  getMemberIdx :: TagName tag -> memberTy -> unionTy -> Integer

instance ( IsTaggedUnion (TaggedUnion members)
         , IsTaggedUnionMember tag memberTy members )
      => HasMember tag memberTy (TaggedUnion members) where
  getMemberIdx = getTagIdx

-- Bits instance
-- =============

-- | Bits instance for tagged unions
instance IsTaggedUnion (TaggedUnion fields) => Bits (TaggedUnion fields) where
  type SizeOf (TaggedUnion fields) =
    Log2Ceil (Length fields) + MaxFieldSize fields
  sizeOf u = log2ceil (getNumMembers u) + getMaxMemberWidth u
  pack u = FromBV $ concatBV u.memberIdx u.memberVal
  unpack inp =
    TaggedUnion {
      memberIdx = selectBV (msbIdx-1, maxMemberWidth) (toBV inp)
    , memberVal = selectBV (maxMemberWidth-1, 0) (toBV inp)
    }
    where
      numMembers = getNumMembers (bottom :: TaggedUnion fields)
      maxMemberWidth = getMaxMemberWidth (bottom :: TaggedUnion fields)
      msbIdx = log2ceil numMembers + maxMemberWidth
  nameBits nm u =
    TaggedUnion {
      memberIdx = addBVNameHint u.memberIdx (NmRoot 0 ("uidx" ++ nm))
    , memberVal = addBVNameHint u.memberVal (NmRoot 0 ("uval" ++ nm))
    }

-- API
-- ===

-- | Does given tagged union have given tag?
infix 7 `isTagged`
isTagged :: forall name memberTy unionTy.
     HasMember name memberTy unionTy
  => unionTy -> TagName name -> Bit 1
isTagged u _ = FromBV $ equalBV (constBV w i) idx
  where
    i = getMemberIdx (bottom :: TagName name)
                     (bottom :: memberTy)
                     (bottom :: unionTy)
    w = log2ceil (getNumMembers (bottom :: unionTy))
    (idx, _) = toRaw u

-- | Shorthand for 'isTagged'
infix 7 `is`
is :: forall name memberTy unionTy.
      HasMember name memberTy unionTy
   => unionTy -> TagName name -> Bit 1
is = isTagged

-- | Construct a value of a tagged union
tag :: forall name memberTy unionTy.
     (HasMember name memberTy unionTy, Bits memberTy)
  => TagName name -> memberTy -> unionTy
tag _ x = fromRaw
            ( constBV (log2ceil numMembers) i
            , zeroExtendBV maxMemberWidth (toBV (pack x)) )
  where
    i = getMemberIdx (bottom :: TagName name)
                     (bottom :: memberTy)
                     (bottom :: unionTy)
    numMembers = getNumMembers (bottom :: unionTy)
    maxMemberWidth = getMaxMemberWidth (bottom :: unionTy)

-- | Get the value of given field if the tag matches.  If tag doesn't
-- match, use optional default value.
untagMaybe :: forall name memberTy unionTy.
     (HasMember name memberTy unionTy, Bits memberTy)
  => TagName name -> Maybe memberTy -> unionTy -> memberTy
untagMaybe _ defaultVal u =
  case defaultVal of
    Nothing -> val
    Just def -> match ? (val, def)
  where
    i = getMemberIdx (bottom :: TagName name)
                     (bottom :: memberTy)
                     (bottom :: unionTy)
    wi = log2ceil numMembers
    numMembers = getNumMembers (bottom :: unionTy)
    maxMemberWidth = getMaxMemberWidth (bottom :: unionTy)
    fieldSize = sizeOf (bottom :: memberTy)
    (rawIdx, rawVal) = toRaw u
    val = unpack $ FromBV $ selectBV (fieldSize-1, 0) rawVal
    match = FromBV $ equalBV (constBV wi i) rawIdx

-- | Get the value of given field if the tag matches.  If tag doesn't
-- match, return 'dontCare'.
untag :: (HasMember name memberTy unionTy, Bits memberTy)
      => TagName name -> unionTy -> memberTy
untag t u = untagMaybe t Nothing u

-- | Get the value of given field if the tag matches.  If tag doesn't
-- match, return default value.
untagDefault :: (HasMember name memberTy unionTy, Bits memberTy)
             => TagName name -> memberTy -> unionTy -> memberTy
untagDefault t defaultVal u = untagMaybe t (Just defaultVal) u

-- | Conditional statement for tagged unions
whenTagged :: (HasMember name memberTy unionTy, Bits memberTy)
           => TagName name
           -> unionTy
           -> (memberTy -> Action a) -> Action a
whenTagged tag u f = whenAction (u `isTagged` tag) (f (untag tag u))

-- FShow instance
-- ==============

-- | For showing tagged unions
class FShowMember a where
  fshowMember :: Integer -> TaggedUnion a -> Format

instance FShowMember '[] where
  fshowMember i _ = mempty

instance (FShow memberTy, Bits memberTy, FShowMember rest, KnownSymbol tag)
      => FShowMember ((tag ::: memberTy) : rest) where
  fshowMember i u =
         formatCond (FromBV cond)
           (fshow "#" <> fshow tagStr <> fshow "("
                      <> fshow memberVal <> fshow ")")
      <> fshowMember (i+1) urest
    where
      cond = equalBV (constBV (bvPrimOutWidth u.memberIdx) i) u.memberIdx
      tagStr = symbolVal (Proxy :: Proxy tag)
      memberVal :: memberTy = unpack (FromBV u.memberVal)
      urest :: TaggedUnion rest =
        TaggedUnion {
          memberIdx = u.memberIdx
        , memberVal = u.memberVal
        }

instance (IsTaggedUnion (TaggedUnion members), FShowMember members)
      => FShow (TaggedUnion members) where
  fshow = fshowMember 0
