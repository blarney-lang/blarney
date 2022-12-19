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

--- Type familes
--- ============

--- | Determine max member size
type family MaxMemberWidth fields where
  MaxMemberWidth '[] = 0
  MaxMemberWidth ((n ::: ty) ': rest) = Max (SizeOf ty) (MaxMemberWidth rest)

-- Type classes
-- ============

-- | Bottom value, used internally
bottom :: a
bottom = error "Blarney.TaggedUnion.bottom"

class IsTaggedUnion t where
  type GetMemberType t (tag :: Symbol) :: Type
  getMembers :: t -> [(String, Int)]
  toRaw :: t -> (BV, BV)
  fromRaw :: (BV, BV) -> t

instance IsTaggedUnion (TaggedUnion '[]) where
  type GetMemberType (TaggedUnion '[]) name = 
    TypeError (Text "TaggedUnion: tag " :<>:
                 ShowType name :<>: Text " doesn't exist")
  getMembers _ = []
  toRaw u = (u.memberIdx, u.memberVal)
  fromRaw (idx, val) = TaggedUnion { memberIdx = idx, memberVal = val }

instance ( Bits t
         , IsTaggedUnion (TaggedUnion rest)
         , KnownSymbol tag
         )
      => IsTaggedUnion (TaggedUnion ((tag ::: t) ': rest)) where
  type GetMemberType (TaggedUnion ((tag ::: t) ': rest)) name =
    If (tag == name) t (GetMemberType (TaggedUnion rest) name)
  getMembers _ =
      (memberName, memberWidth) : getMembers (bottom :: TaggedUnion rest)
    where
      memberName = symbolVal (Proxy :: Proxy tag)
      memberWidth = sizeOf (bottom :: t)
  toRaw u = (u.memberIdx, u.memberVal)
  fromRaw (idx, val) = TaggedUnion { memberIdx = idx, memberVal = val }

-- Helper functions
getNumMembers :: IsTaggedUnion u => u -> Int
getNumMembers = length . getMembers

getMaxMemberWidth :: IsTaggedUnion u => u -> Int
getMaxMemberWidth = maximum . map snd . getMembers

getMemberIdx :: IsTaggedUnion u => u -> String -> Integer
getMemberIdx u tag = 
  case [idx | ((n, w), idx) <- zip members [0..], n == tag] of
    [idx] -> idx
    _ -> error ("Blarney.TaggedUnion.getMemberIdx: " ++ tag)
  where
    members = getMembers u

-- Bits instance
-- =============

-- | Bits instance for tagged unions
instance IsTaggedUnion (TaggedUnion fields) => Bits (TaggedUnion fields) where
  type SizeOf (TaggedUnion fields) =
    Log2Ceil (Length fields) + MaxMemberWidth fields
  sizeOf u = log2ceil (length members) + maximum (map snd members)
    where members = getMembers u
  pack u = FromBV $ concatBV u.memberIdx u.memberVal
  unpack inp =
    TaggedUnion {
      memberIdx = selectBV (msbIdx-1, maxMemberWidth) (toBV inp)
    , memberVal = selectBV (maxMemberWidth-1, 0) (toBV inp)
    }
    where
      members = getMembers (bottom :: TaggedUnion fields)
      maxMemberWidth = maximum (map snd members)
      msbIdx = log2ceil (length members) + maxMemberWidth
  nameBits nm u =
    TaggedUnion {
      memberIdx = addBVNameHint u.memberIdx (NmRoot 0 ("utag" ++ nm))
    , memberVal = addBVNameHint u.memberVal (NmRoot 0 ("uval" ++ nm))
    }

-- API
-- ===

-- | Does given tagged union have given tag?
infix 7 `isTagged`
isTagged :: forall u name.
     (IsTaggedUnion u, KnownSymbol name)
  => u -> TagName name -> Bit 1
isTagged u _ = FromBV $ equalBV (constBV w i) idx
  where
    str = symbolVal (Proxy :: Proxy name)
    i   = getMemberIdx u str
    w   = log2ceil (getNumMembers u)
    (idx, _) = toRaw u

-- | Shorthand for 'isTagged'
infix 7 `is`
is :: forall u name.
      (IsTaggedUnion u, KnownSymbol name)
   => u -> TagName name -> Bit 1
is = isTagged

-- | Construct a value of a tagged union
tag :: forall name m u.
       (IsTaggedUnion u, KnownSymbol name, m ~ GetMemberType u name, Bits m)
    => TagName name -> m -> u
tag _ x = fromRaw
            ( constBV (log2ceil numMembers) i
            , zeroExtendBV maxMemberWidth (toBV (pack x)) )
  where
    str = symbolVal (Proxy :: Proxy name)
    i = getMemberIdx (bottom :: u) str
    numMembers = getNumMembers (bottom :: u)
    maxMemberWidth = getMaxMemberWidth (bottom :: u)

-- | Get the value of given field if the tag matches.  If tag doesn't
-- match, use optional default value.
untagMaybe :: forall name m u.
     (IsTaggedUnion u, KnownSymbol name, m ~ GetMemberType u name, Bits m)
  => TagName name -> Maybe m -> u -> m
untagMaybe _ defaultVal u =
  case defaultVal of
    Nothing -> val
    Just def -> match ? (val, def)
  where
    str = symbolVal (Proxy :: Proxy name)
    i = getMemberIdx u str
    wi = log2ceil numMembers
    numMembers = getNumMembers u
    maxMemberWidth = getMaxMemberWidth u
    fieldSize = sizeOf (bottom :: m)
    (rawIdx, rawVal) = toRaw u
    val = unpack $ FromBV $ selectBV (fieldSize-1, 0) rawVal
    match = FromBV $ equalBV (constBV wi i) rawIdx

-- | Get the value of given field if the tag matches.  If tag doesn't
-- match, return 'dontCare'.
untag :: (IsTaggedUnion u, KnownSymbol name, m ~ GetMemberType u name, Bits m)
      => TagName name -> u -> m
untag t u = untagMaybe t Nothing u

-- | Get the value of given field if the tag matches.  If tag doesn't
-- match, return default value.
untagDefault :: forall name m u.
     (IsTaggedUnion u, KnownSymbol name, m ~ GetMemberType u name, Bits m)
  => TagName name -> m -> u -> m
untagDefault t defaultVal u = untagMaybe t (Just defaultVal) u

-- | Conditional statement for tagged unions
whenTagged ::
     (IsTaggedUnion u, KnownSymbol name, m ~ GetMemberType u name, Bits m)
  => TagName name
  -> u
  -> (GetMemberType u name -> Action a) -> Action a
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

-- Interface instance
-- ==================

instance IsTaggedUnion (TaggedUnion members)
      => Interface (TaggedUnion members) where
  toIfc u = (tm, ty)
    where
      tm = IfcTermProduct (IfcTermBV u.memberIdx)
                          (IfcTermBV u.memberVal)
      ty = IfcTypeProduct (IfcTypeField (portName "tag") (IfcTypeBV idxWidth))
                          (IfcTypeField (portName "val") (IfcTypeBV valWidth))
      idxWidth = log2ceil (getNumMembers u)
      valWidth = getMaxMemberWidth u
  fromIfc ~(IfcTermProduct (IfcTermBV idx) (IfcTermBV val)) =
    TaggedUnion { memberIdx = idx, memberVal = val }
