{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-|
Module      : Blarney.Core.Interface
Description : Support for separate compilation
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2020
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

Rather than flattening the entire design hierarchy down to a single netlist, we
want to maintain the modular structure when generating external modules.
'Blarney.Core.Interface' allows Blarney functions to be turned into external
modules, and external modules to be instantiated in a Blarney description.
-}
module Blarney.Core.Interface (
  Interface(..) -- Types that can be converted to external module I/O ports
, IfcTerm(..)   -- Generic term representation
, IfcType(..)   -- Generic type representation
, Method(..)    -- Function types convertible to external module I/O ports
, Modular(..)   -- Types that can be turned into external modules
, makeModule    -- Convert a Blarney function to an external module
, makeInstance  -- Instantiate an external module in a Blarney description
, makeInstanceOf -- Use type of argument rather than needing type sig
, makeInstanceWithParams  -- Allow static parameters
) where

-- Standard imports
import Prelude
import GHC.TypeLits
import GHC.Generics
import Control.Monad.Fix
import Control.Monad hiding (when)
import Data.List (intersperse)
import Data.Proxy

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Prim
import Blarney.Core.Module
import Blarney.Core.Prelude
import qualified Blarney.Core.RTL as RTL

-- Interface term representation
data IfcTerm =
    IfcTermUnit
  | IfcTermBV BV
  | IfcTermAction (Action IfcTerm)
  | IfcTermProduct [IfcTerm]
  | IfcTermFun (IfcTerm -> IfcTerm)

-- Interface type representation
data IfcType =
    IfcTypeUnit
  | IfcTypeBV Width
  | IfcTypeAction IfcType
  | IfcTypeProduct [IfcType]
  | IfcTypeFun IfcType IfcType
    -- Marks new field selector with name
  | IfcTypeMetaSel String IfcType

class Interface a where
  toIfcTerm   :: a -> IfcTerm
  fromIfcTerm :: IfcTerm -> a
  toIfcType   :: a -> IfcType

  -- |For generic deriving
  default toIfcTerm :: (Generic a, GInterface (Rep a)) => a -> IfcTerm
  toIfcTerm x = gtoIfcTerm (from x)
  default fromIfcTerm :: (Generic a, GInterface (Rep a)) => IfcTerm -> a
  fromIfcTerm x = to (gfromIfcTerm x)
  default toIfcType :: (Generic a, GInterface (Rep a)) => a -> IfcType
  toIfcType x = gtoIfcType (from x)

instance KnownNat n => Interface (Bit n) where
  toIfcTerm x = IfcTermBV (toBV x)
  fromIfcTerm ~(IfcTermBV x) = FromBV x
  toIfcType x = IfcTypeBV (fromIntegral (widthOf x))

instance Interface a => Interface (Action a) where
  toIfcTerm act = IfcTermAction (toIfcTerm <$> act)
  fromIfcTerm ~(IfcTermAction act) = fromIfcTerm <$> act
  toIfcType _ = IfcTypeAction (toIfcType (undefined :: a))

instance (Interface a, Bits a, Method b) => Interface (a -> b) where
  toIfcTerm = toMethodTerm
  fromIfcTerm = fromMethodTerm
  toIfcType = toMethodType

-- |This class defines which functions make a valid 'Interface'.
-- Specifially, a 'Method' is any function returning an 'Action'
-- whose arguments are in 'Interface' and 'Bits'.  Note that
-- we don't actually use the 'Bits' dictionary, but it captures at
-- the type level the restriction that method arguments must be
-- assignable at the circuit level.
class Method a where
  toMethodTerm :: a -> IfcTerm
  fromMethodTerm :: IfcTerm -> a
  toMethodType :: a -> IfcType

instance Interface a => Method (Action a) where
  toMethodTerm = toIfcTerm
  fromMethodTerm = fromIfcTerm
  toMethodType = toIfcType

instance (Interface a, Bits a, Method b) => Method (a -> b) where
  toMethodTerm f = IfcTermFun (\a -> toMethodTerm (f (fromIfcTerm a)))
  fromMethodTerm ~(IfcTermFun f) = \a -> fromMethodTerm (f (toIfcTerm a))
  toMethodType _ = IfcTypeFun (toIfcType (undefined :: a))
                              (toMethodType (undefined :: b))

-- |For generic deriving of 'Interface'
class GInterface f where
  gtoIfcTerm   :: f a -> IfcTerm
  gfromIfcTerm :: IfcTerm -> f a
  gtoIfcType   :: f a -> IfcType

instance GInterface U1 where
  gtoIfcTerm _ = IfcTermUnit
  gfromIfcTerm _ = U1
  gtoIfcType _ = IfcTypeUnit

instance (GInterface a, GInterface b) => GInterface (a :*: b) where
  gtoIfcTerm ~(a :*: b) = IfcTermProduct [gtoIfcTerm a, gtoIfcTerm b]
  gfromIfcTerm ~(IfcTermProduct [a, b]) = gfromIfcTerm a :*: gfromIfcTerm b
  gtoIfcType ~(a :*: b) = IfcTypeProduct [gtoIfcType a, gtoIfcType b]

instance (GInterface a, Selector c) => GInterface (M1 S c a) where
  gtoIfcTerm ~(m@(M1 x)) = gtoIfcTerm x
  gfromIfcTerm x = M1 (gfromIfcTerm x)
  gtoIfcType ~(m@(M1 x)) = IfcTypeMetaSel (selName m) (gtoIfcType x)

instance {-# OVERLAPPABLE #-} GInterface a => GInterface (M1 i c a)  where
  gtoIfcTerm ~(m@(M1 x)) = gtoIfcTerm x
  gfromIfcTerm x = M1 (gfromIfcTerm x)
  gtoIfcType ~(m@(M1 x)) = gtoIfcType x

instance Interface a => GInterface (K1 i a) where
  gtoIfcTerm ~(K1 x) = toIfcTerm x
  gfromIfcTerm x = K1 (fromIfcTerm x)
  gtoIfcType ~(K1 x) = toIfcType x

-- Instances
instance Interface ()
instance (Interface a, Interface b) => Interface (a, b)
instance (Interface a, Interface b, Interface c) => Interface (a, b, c)
instance (Interface a, Interface b, Interface c, Interface d) =>
  Interface (a, b, c, d)
instance (Interface a, Interface b, Interface c, Interface d, Interface e) =>
  Interface (a, b, c, d, e)
instance (Interface a, Interface b, Interface c, Interface d,
          Interface e, Interface f) =>
  Interface (a, b, c, d, e, f)
instance (Interface a, Interface b, Interface c, Interface d,
          Interface e, Interface f, Interface g) =>
  Interface (a, b, c, d, e, f, g)
instance (Interface a, Bits a) => Interface (Reg a)
instance (Interface a, Bits a) => Interface (Wire a)
instance (Interface a, Bits a) => Interface (ReadWrite a)
instance (Interface a, Bits a) => Interface (WriteOnly a)

-- |Declaration reader/writer monad for declaring named inputs and outputs.
-- The writer part of the monad collects declarations.  Any names
-- declared as inputs are fed cyclically back into the reader part of
-- the monad in the form of an environment mapping input names to
-- bit-vectors.
newtype Declare a =
  Declare {
    runDeclare :: Int -> Scope -> Env -> Module (Int, [Decl], a)
  }

-- Environment mapping string names to bit-vector values
type Env = [(String, BV)]

-- Declarations being collected
-- (A named input with a width, or a named output bit-vector)
data Decl = DeclInput String Width | DeclOutput String BV

-- A scope is a stack of names, used to generate sensible names
type Scope = [String]

-- Instances of Monad, Applicable, Functor
instance Monad Declare where
  return a = Declare \c s e -> return (c, [], a)
  m >>= f = Declare \c s e -> noName do
    (c', w0, a) <- runDeclare m c s e
    (c'', w1, b) <- runDeclare (f a) c' s e
    return (c'', w0 ++ w1, b)

instance Applicative Declare where
  pure = return
  (<*>) = ap

instance Functor Declare where
  fmap = liftM

-- Lookup name in environment
lookupInputBV :: String -> Declare BV
lookupInputBV name = Declare $ \c s e ->
  let bv = case lookup name e of
             Nothing ->
               error ("Interface.lookupInputBV: key not found: " ++ name)
             Just bv -> bv
  in  return (c, [], bv)

-- Start a new scope
newScope :: String -> Declare a -> Declare a
newScope name m = Declare \c s e -> noName do
  let name' = case name of { "" -> show c; other -> name }
  (_, ds, a) <- runDeclare m 0 (name':s) e
  return (c+1, ds, a)

-- Lift module monad to declare monad
liftModule :: Module a -> Declare a
liftModule m = Declare \c s e -> noName do
  a <- m
  return (c, [], a)

-- Get a meaningful name with given prefix
getName :: Declare String
getName = Declare \c s e ->
  return (c, [], concat (intersperse "_" (reverse s)))

-- Add a declaration to the collection
addDecl :: Decl -> Declare ()
addDecl d = Declare \c s e -> return (c, [d], ())

-- Declare given bit-vector as an output
declareOutputBV :: String -> BV -> Declare ()
declareOutputBV suffix bv = do
  nm <- getName
  let name = case suffix of { "" -> nm; other -> nm ++ "_" ++ suffix }
  addDecl (DeclOutput name bv)

-- Declare a new input bit-vector of given width
declareInputBV :: String -> Width -> Declare BV
declareInputBV suffix width = do
  nm <- getName
  let name = case suffix of { "" -> nm; other -> nm ++ "_" ++ suffix }
  addDecl (DeclInput name width)
  lookupInputBV name

-- Declare an output, generically over any interface type
declareOut :: IfcTerm -> IfcType -> Declare ()
declareOut x (IfcTypeMetaSel selName t) =
  newScope selName (declareOut x t)
declareOut IfcTermUnit _ = return ()
declareOut (IfcTermBV bv) _ =
  declareOutputBV "" bv
declareOut (IfcTermAction act) (IfcTypeAction t) = do
  -- Declare input wire to trigger execution of act
  en <- declareInputBV "en" 1
  -- Trigger action
  ret <- liftModule $ always $ whenR (FromBV en :: Bit 1) act
  -- Declare return value output
  newScope "ret" (declareOut ret t)
declareOut (IfcTermProduct xs) (IfcTypeProduct txs) =
  zipWithM_ declareOut xs txs
declareOut (IfcTermFun fun) (IfcTypeFun argType retType) = do
  -- Declare argument as input
  arg <- newScope "" (declareIn argType)
  -- Apply argument and declare return value as output
  declareOut (fun arg) retType

-- Typed version of 'declareOut'
declareOutput :: Interface a => String -> a -> Declare ()
declareOutput str out =
  newScope str (declareOut (toIfcTerm out) (toIfcType out))

-- Declare an input, generically over any interface type
declareIn :: IfcType -> Declare IfcTerm
declareIn (IfcTypeMetaSel selName t) =
  newScope selName (declareIn t)
declareIn IfcTypeUnit = return IfcTermUnit
declareIn (IfcTypeBV w) = do
  bv <- declareInputBV "" w
  return (IfcTermBV bv)
declareIn (IfcTypeAction t) = do
  -- Declare return value as input
  ret <- newScope "ret" (declareIn t)
  -- Create enable wire
  enWire :: Wire (Bit 1) <- liftModule (makeWire 0)
  -- Declare enable signal as output
  declareOutputBV "en" (toBV (val enWire))
  -- When action block is called, trigger the enable line
  return (IfcTermAction $ do { enWire <== 1; return ret })
declareIn (IfcTypeProduct ts) =
  IfcTermProduct <$> mapM declareIn ts
declareIn t@(IfcTypeFun _ _) = do
  let (argTypes, retType) = flatten t
  -- The return type must be an action (the Method class captures
  -- this assumption, but let's check anyway)
  case retType of
    IfcTypeAction{} -> do
      -- Declare each argument as an output
      drivers <- forM argTypes \argType ->
        newScope "" (driver argType)
      -- Declare return type as input
      ret <- declareIn retType
      case ret of
        IfcTermAction retAct -> do
          -- Construct a function which assigns the args and
          -- executes the return action
          let driveArgs act (driver:rest) =
                IfcTermFun (\x -> driveArgs (driver x >> act) rest)
              driveArgs act [] = IfcTermAction act
          return (driveArgs retAct drivers)
        other -> error "Blarney.Core.Interface: maltyped term"
    other -> error "Blarney.Core.Interface: method constraint violated"
  where
    -- Flatten arrow type to a list of argument types and a return type
    flatten :: IfcType -> ([IfcType], IfcType)
    flatten (IfcTypeFun argType retType) = (argType : args, ret)
      where (args, ret) = flatten retType
    flatten other = ([], other)

    -- Declare an output and return assignment
    -- function (driver) for that output
    driver :: IfcType -> Declare (IfcTerm -> Action ())
    driver (IfcTypeMetaSel selName t) = newScope selName (driver t)
    driver IfcTypeUnit = return (\x -> return ())
    driver (IfcTypeBV w) =
      liftNat w $ \(_ :: Proxy n) -> do
        -- Create assignable wire for this bit-vector
        wire :: Wire (Bit n) <- liftModule (makeWire dontCare)
        -- Declare wire value as an output
        declareOutputBV "" (toBV (val wire))
        -- Return assigner
        return (\(IfcTermBV bv) -> wire <== FromBV bv)
    driver (IfcTypeProduct ts) = do
      drivers <- mapM driver ts
      return (\(IfcTermProduct xs) -> zipWithM_ ($) drivers xs)
    driver other =
      error "Blarney.Core.Interface: driver applied to non-Bits type"

-- Typed version of 'declareIn'
declareInput :: forall a. Interface a => String -> Declare a
declareInput str =
  newScope str do
    let t = toIfcType (undefined :: a)
    x <- declareIn t
    return (fromIfcTerm x)

class Modular a where
  makeMod :: Int -> a -> Declare ()
  makeInst :: String -> [Param] -> Int -> Declare () -> a

-- Specific base case 
instance {-# OVERLAPPING #-} Interface a => Modular (Module a) where
  makeMod count m =
    liftModule m >>= declareOutput "out"
  makeInst s ps count m =
    instantiate s ps True (m >> declareInput "out")

-- Specific recursive case
instance {-# OVERLAPPING #-} (Interface a, Modular m) => Modular (a -> m) where
  makeMod count f = do
    a <- declareInput ("in" ++ show count)
    makeMod (count+1) (f a)
  makeInst s ps count m = \a ->
    makeInst s ps (count+1) (m >> declareOutput ("in" ++ show count) a)

-- General base case (pure function)
instance {-# OVERLAPPABLE #-} Interface a => Modular a where
  makeMod count out = declareOutput "out" out
  makeInst s ps count m = runPureModule mod
      "Interface.makeInstance: function must be in the Module monad, or pure"
    where
      mod = instantiate s ps False (m >> declareInput "out")

-- Realise declarations to give standalone module
modularise :: Declare a -> Module a
modularise ifc = noName mdo
    (_, w, a) <- runDeclare ifc 0 [] inps
    inps <- mod w
    return a
  where
    mod w = noName do
      let outputs = [(s, x) | (DeclOutput s x) <- w]
      let inputs  = [(s, n) | DeclInput s n <- w]
      mapM (\(s,x) -> outputBV s x) outputs
      tmps <- mapM (\(s,n) -> inputBV s n) inputs
      return $ zip (map fst inputs) tmps

-- Realise declarations to give module instance
instantiate :: String -> [Param] -> Bool -> Declare a -> Module a
instantiate name params doAddRoots ifc = noName mdo
    (_, w, a) <- runDeclare ifc 0 [] (custom w)
    addRoots [x | DeclOutput s x <- w, doAddRoots]
    return a
  where
    custom w =
      let inputs   = [(s, x) | (DeclOutput s x) <- w]
          outputs  = [(s, n) | DeclInput s n <- w]
          outNames = map fst outputs
          prim     = Custom name [(s, bvPrimOutWidth x) | (s, x) <- inputs]
                           outputs params True
      in  zip outNames (makePrim prim (map snd inputs) (map Just outNames))

makeModule :: Modular a => a -> Module ()
makeModule a = modularise (makeMod 0 a)

makeInstanceWithParams :: Modular a => String -> [Param] -> a
makeInstanceWithParams s ps = makeInst s ps 0 (return ())

makeInstance :: Modular a => String -> a
makeInstance s = makeInstanceWithParams s []

-- | Use type of argument to specify type of result
makeInstanceOf :: Modular a => a -> String -> a
makeInstanceOf _ s = makeInstanceWithParams s []
