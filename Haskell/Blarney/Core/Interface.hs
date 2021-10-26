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
{-# LANGUAGE NoRebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-|
Module      : Blarney.Core.Interface
Description : Support for separate compilation
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

Rather than flattening the entire design hierarchy down to a single netlist, we
want to maintain the modular structure when generating external modules.
'Blarney.Core.Interface' allows Blarney functions to be turned into external
modules, and external modules to be instantiated in a Blarney description.
-}
module Blarney.Core.Interface (
  -- Class of types that can be converted to external module I/O ports
  Interface(..)
  -- Generic term representation
, IfcTerm(..)
  -- Generic type representation
, IfcType(..)
  -- Function types convertible to external module I/O ports
, Method(..)
  -- Types that can be turned into external modules
, Modular(..)
  -- Information about an instance
, InstanceInfo(..)
  -- Convert a Blarney function to an external module
, makeModule
  -- Instantiate an external module in a Blarney description
, makeInstanceWithInfo
  -- Instantiate module with default info
, makeInstance
  -- Introduce synthesis boundary
, makeBoundaryWithInfo
  -- Introduce synthesis boundary with default info
, makeBoundary
  -- Introduce synthesis boundary, with instance taking given clock and reset
, makeBoundaryWithClockAndReset
) where

-- Standard imports
import Prelude
import GHC.TypeLits
import GHC.Generics
import Control.Monad.Fix
import Control.Monad hiding (when)
import Data.List (intersperse)
import Data.Proxy
import Data.Maybe

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Prim
import Blarney.Core.Module
import Blarney.Core.Prelude
import Blarney.Core.Flatten
import Blarney.Core.ClockReset
import qualified Blarney.Core.RTL as RTL

-- Interface term representation
data IfcTerm =
    IfcTermUnit
  | IfcTermBV BV
  | IfcTermAction (Action IfcTerm)
  | IfcTermProduct IfcTerm IfcTerm
  | IfcTermFun (IfcTerm -> IfcTerm)

-- Interface type representation
data IfcType =
    IfcTypeUnit
  | IfcTypeBV Width
  | IfcTypeAction IfcType
  | IfcTypeProduct IfcType IfcType
  | IfcTypeFun IfcType IfcType
    -- Marks each field / constructor argument (with name, which can be "")
  | IfcTypeField String IfcType

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
  gtoIfcTerm ~(a :*: b) = IfcTermProduct (gtoIfcTerm a) (gtoIfcTerm b)
  gfromIfcTerm ~(IfcTermProduct a b) = gfromIfcTerm a :*: gfromIfcTerm b
  gtoIfcType ~(a :*: b) = IfcTypeProduct (gtoIfcType a) (gtoIfcType b)

instance (GInterface a, Selector c) => GInterface (M1 S c a) where
  gtoIfcTerm ~(m@(M1 x)) = gtoIfcTerm x
  gfromIfcTerm x = M1 (gfromIfcTerm x)
  gtoIfcType ~(m@(M1 x)) = IfcTypeField (selName m) (gtoIfcType x)

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
instance Interface Clock
instance Interface Reset

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

-- Get a meaningful name
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
declareOut x (IfcTypeField selName t) =
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
declareOut (IfcTermProduct x0 x1) (IfcTypeProduct tx0 tx1) = do
  declareOut x0 tx0
  declareOut x1 tx1
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
declareIn (IfcTypeField selName t) =
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
declareIn (IfcTypeProduct t0 t1) =
  IfcTermProduct <$> declareIn t0 <*> declareIn t1
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
    driver (IfcTypeField selName t) = newScope selName (driver t)
    driver IfcTypeUnit = return (\x -> return ())
    driver (IfcTypeBV w) =
      liftNat w $ \(_ :: Proxy n) -> do
        -- Create assignable wire for this bit-vector
        wire :: Wire (Bit n) <- liftModule (makeWire dontCare)
        -- Declare wire value as an output
        declareOutputBV "" (toBV (val wire))
        -- Return assigner
        return (\(IfcTermBV bv) -> wire <== FromBV bv)
    driver (IfcTypeProduct t0 t1) = do
      driver0 <- driver t0
      driver1 <- driver t1
      return (\(IfcTermProduct x0 x1) -> driver0 x0 >> driver1 x1)
    driver other =
      error "Blarney.Core.Interface: driver applied to non-Bits type"

-- Typed version of 'declareIn'
declareInput :: forall a. Interface a => String -> Declare a
declareInput str =
  newScope str do
    let t = toIfcType (undefined :: a)
    x <- declareIn t
    return (fromIfcTerm x)

-- Information about an instance
data InstanceInfo =
  InstanceInfo {
    instanceParams :: [Param]
  , instanceClock :: Maybe Clock
  , instanceReset :: Maybe Reset
  }

-- Default instance information
defaultInstanceInfo :: InstanceInfo
defaultInstanceInfo =
  InstanceInfo {
    instanceParams = []
  , instanceClock = Nothing
  , instanceReset = Nothing
  }

class Modular a where
  makeMod :: Int -> a -> Declare ()
  makeInst :: String -> InstanceInfo -> Int
           -> Declare () -> Maybe CustomNetlist -> a

-- Specific base case
instance {-# OVERLAPPING #-} Interface a => Modular (Module a) where
  makeMod count m =
    liftModule m >>= declareOutput "out"
  makeInst s info count m mcnl =
    instantiate s info True (m >> declareInput "out") mcnl

-- Specific recursive case
instance {-# OVERLAPPING #-} (Interface a, Modular m) => Modular (a -> m) where
  makeMod count f = do
    a <- declareInput ("in" ++ show count)
    makeMod (count+1) (f a)
  makeInst s info count m mcnl = \a ->
    makeInst s info (count+1) (m >> declareOutput ("in" ++ show count) a) mcnl

-- General base case (pure function)
instance {-# OVERLAPPABLE #-} Interface a => Modular a where
  makeMod count out = declareOutput "out" out
  makeInst s info count m mcnl = runPureModule mod
      "Interface.makeInstance: function must be in the Module monad, or pure"
    where
      mod = instantiate s info False (m >> declareInput "out") mcnl

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
instantiate :: String -> InstanceInfo -> Bool -> Declare a
            -> Maybe CustomNetlist
            -> Module a
instantiate name info doAddRoots ifc mcnl = noName mdo
    let (bvs, env) = case custom w of Left bv -> ([bv], [])
                                      Right e -> (snd <$> e, e)
    (_, w, a) <- runDeclare ifc 0 [] env
    addRoots [x | x <- bvs, doAddRoots]
    return a
  where
    custom w =
      let inputs   = [ ("clock", toBV clk)
                     | Just (Clock clk) <- [instanceClock info] ]
                  ++ [ ("reset", toBV rst)
                     | Just (Reset rst) <- [instanceReset info] ]
                  ++ [(s, x) | (DeclOutput s x) <- w]
          outputs  = [(s, n) | DeclInput s n <- w]
          outNames = map fst outputs
          addClock = isNothing (instanceClock info)
          addReset = isNothing (instanceReset info)
          prim     = Custom name [(s, bvPrimOutWidth x) | (s, x) <- inputs]
                           outputs (instanceParams info)
                             addClock addReset mcnl
      in if null outputs
           then Left $ makePrim0 prim (map snd inputs)
           else Right . zip outNames $ makePrim prim (map snd inputs)
                                                     (map Just outNames)

makeModule :: Modular a => a -> Module ()
makeModule a = modularise (makeMod 0 a)

makeInstanceWithInfo :: Modular a =>
     String              -- ^ Name of component being instantiated
  -> InstanceInfo        -- ^ Instance information
  -> Maybe CustomNetlist -- ^ Optional netlist for component
  -> a
makeInstanceWithInfo s info mcnl =
  makeInst s info 0 (return ()) mcnl

makeInstance :: Modular a => String -> a
makeInstance s = makeInstanceWithInfo s defaultInstanceInfo Nothing

-- | Make a named synthesis boundary around the given module.
-- Note that it is possible for the supplied module to capture data that
-- is independent of the module's inputs; this may or may not be
-- desirable, so take care.
makeBoundaryWithInfo :: Modular a => InstanceInfo -> String -> a -> a
makeBoundaryWithInfo info name m = makeInstanceWithInfo name info mcnl
  where mcnl = Just $ CustomNetlist $ toNetlist $ makeModule m

makeBoundary :: Modular a => String -> a -> a
makeBoundary = makeBoundaryWithInfo defaultInstanceInfo

makeBoundaryWithClockAndReset :: Modular a => (Clock, Reset) -> String -> a -> a
makeBoundaryWithClockAndReset (clk, rst) =
  makeBoundaryWithInfo
    defaultInstanceInfo {
      instanceClock = Just clk
    , instanceReset = Just rst
    }
