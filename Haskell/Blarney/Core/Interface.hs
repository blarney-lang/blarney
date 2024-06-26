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
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
  -- Generic term & type representation
, IfcTerm(..)
, IfcType(..)
, toIfcTerm
, fromIfcTerm
, toIfcType
  -- Syntactic sugar for making manual Interface instances
, toPorts
, fromPorts
, PortInfo(..)
, portEmpty
, portName
, portMethod
, portMethodEn
, portMethodAlwaysEn
, portSkipName
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
import Blarney.Core.Common
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
  | IfcTypeField PortInfo IfcType

-- | The user can request certain properties, e.g. names, in a generated
-- flat interface (e.g. Verilog). Such properties are captured per-port as:
data PortInfo =
  PortInfo {
    name :: String
    -- ^ Desired field name
  , argNames :: [String]
    -- ^ Desired method argument names
  , alwaysEnabled :: Bool
    -- ^ Avoid generation of method enable wire
  , skipName :: Bool
    -- ^ Do not generate a name for this port. Useful when we are only
    -- intested in a method's arguments. Use with care; this can
    -- introduce ambiguous names in generated interface.
  , enableName :: String
    -- ^ Name extension for a method's enable wire
  }

-- | Empty information about a port
portEmpty :: PortInfo
portEmpty =
  PortInfo { name = "", argNames = []
           , alwaysEnabled = False, skipName = False
           , enableName = "en" }

-- | Name a port
portName :: String -> PortInfo
portName nm = portEmpty { name = nm }

-- | Name a method port, i.e. the method and its args
portMethod :: String -> [String] -> PortInfo
portMethod m args = portEmpty { name = m, argNames = args, skipName = null m }

-- | Name a method port and its enable wire
portMethodEn :: String -> String -> [String] -> PortInfo
portMethodEn m en args =
  portEmpty { name = m, argNames = args, skipName = null m, enableName = en }

-- | Name an always-enabled method port
portMethodAlwaysEn :: String -> [String] -> PortInfo
portMethodAlwaysEn m args = (portMethod m args) { alwaysEnabled = True }

-- | Skip name augmentation for a port
portSkipName :: PortInfo
portSkipName = portEmpty { skipName = True }

class Interface a where
  toIfc :: a -> (IfcTerm, IfcType)
  fromIfc :: IfcTerm -> a

  -- |For generic deriving
  default toIfc :: (Generic a, GInterface (Rep a)) => a -> (IfcTerm, IfcType)
  toIfc x = (gtoIfcTerm (from x), gtoIfcType (from x))
  default fromIfc :: (Generic a, GInterface (Rep a)) => IfcTerm -> a
  fromIfc x = to (gfromIfcTerm x)

toIfcTerm :: Interface a => a -> IfcTerm
toIfcTerm x = fst (toIfc x)

toIfcType :: Interface a => a -> IfcType
toIfcType x = snd (toIfc x)

fromIfcTerm :: Interface a => IfcTerm -> a
fromIfcTerm x = fromIfc x

instance KnownNat n => Interface (Bit n) where
  toIfc x =(tm, ty)
    where
      tm = IfcTermBV (toBV x)
      ty = IfcTypeBV (fromIntegral (widthOf x))
  fromIfc ~(IfcTermBV x) = FromBV x

instance KnownNat n => Interface (Signed n) where
  toIfc x = (tm, ty)
    where
      tm = toIfcTerm (fromSigned x)
      ty = toIfcType (fromSigned x)
  fromIfc x = toSigned (fromIfc x)

instance Interface a => Interface (Action a) where
  toIfc act = (tm, ty)
    where
      tm = IfcTermAction (toIfcTerm <$> act)
      ty = IfcTypeAction (toIfcType (undefined :: a))
  fromIfc ~(IfcTermAction act) = fromIfc <$> act

instance (Interface a, Bits a, Method b) => Interface (a -> b) where
  toIfc x = (toMethodTerm x, toMethodType x)
  fromIfc = fromMethodTerm

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
  gtoIfcType ~(m@(M1 x)) = IfcTypeField (portName (selName m)) (gtoIfcType x)

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

-- Sugar for writing manual interface instances
class ToPorts a where
  toPortsAcc :: [(IfcTerm, IfcType)] -> a

instance ToPorts (IfcTerm, IfcType) where
  toPortsAcc acc = (tm, ty)
    where
      tm = foldr IfcTermProduct IfcTermUnit tms
      ty = foldr IfcTypeProduct IfcTypeUnit tys
      (tms, tys) = unzip (reverse acc)

instance (Interface a, ToPorts b) => ToPorts ((PortInfo, a) -> b) where
  toPortsAcc acc = \(info, x) ->
    let (tm_x, ty_x) = toIfc x in
      toPortsAcc ((tm_x, IfcTypeField info ty_x) : acc)

toPorts :: ToPorts a => a
toPorts = toPortsAcc []

class FromPorts a b where
  fromPorts :: IfcTerm -> a -> b

instance {-# OVERLAPPABLE #-} a ~ b => FromPorts a b where
  fromPorts _ x = x

instance (Interface a, FromPorts b c) => FromPorts (a -> b) c where
  fromPorts ~(IfcTermProduct tm0 tm1) f =
    fromPorts tm1 (f (fromIfc tm0))

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

-- As we flatten a high-level (hiearchical) interface to a low-level
-- (flat) one, we track port information along the current path
type Scope = [PortInfo]

-- Instances of Monad, Applicable, Functor
instance Monad Declare where
  return = pure
  m >>= f = Declare \c s e -> noName do
    (c', w0, a) <- runDeclare m c s e
    (c'', w1, b) <- runDeclare (f a) c' s e
    return (c'', w0 ++ w1, b)

instance Applicative Declare where
  pure a = Declare \c s e -> return (c, [], a)
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


-- Get current arg counter
getArgCounter :: Declare Int
getArgCounter = Declare \c s e -> return (c, [], c)

-- Get scope
getScope :: Declare Scope
getScope = Declare \c s e -> return (c, [], s)

-- Start a new scope
newScope :: PortInfo -> Declare a -> Declare a
newScope info m = Declare \c s e -> noName do
  let nm = case info.name of { "" -> show c; other -> info.name }
  let newInfo = if info.skipName then info else info { name = nm }
  (_, ds, a) <- runDeclare m 0 (newInfo:s) e
  return (c+1, ds, a)

-- Lift module monad to declare monad
liftModule :: Module a -> Declare a
liftModule m = Declare \c s e -> noName do
  a <- m
  return (c, [], a)

-- Get a meaningful name
getName :: Declare String
getName = do
  s <- getScope 
  return (concat $ intersperse "_"
                 $ filter (not . null)
                 $ map (.name)
                 $ reverse s)

-- Get method argument name
getArgName :: Declare String
getArgName = do
  i <- getArgCounter
  s <- getScope
  let nm = case s of
             pi:_ | i < length pi.argNames -> pi.argNames !! i
             other -> show i
  return nm

-- Dertmine if current method being processed is always enabled
getAlwaysEnabled :: Declare Bool
getAlwaysEnabled = do
  s <- getScope
  case s of
    PortInfo { alwaysEnabled = True } : _ -> return True
    _ -> return False

-- Determine name for enable wire of current method being processed
getMethodEnableName :: Declare String
getMethodEnableName = do
  s <- getScope
  case s of
    pi:_ -> return pi.enableName
    _ -> return "en"

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
declareOut x (IfcTypeField info t) =
  newScope info (declareOut x t)
declareOut IfcTermUnit _ = return ()
declareOut (IfcTermBV bv) _ =
  declareOutputBV "" bv
declareOut (IfcTermAction act) (IfcTypeAction t) = do
  alwaysEn <- getAlwaysEnabled
  enName <- getMethodEnableName
  if alwaysEn
    then do
      -- Trigger action
      ret <- liftModule (always act)
      -- Declare return value output
      newScope (portName "ret") (declareOut ret t)
    else do
      -- Declare input wire to trigger execution of act
      en <- declareInputBV enName 1
      -- Trigger action
      ret <- liftModule $ always $ whenAction (FromBV en :: Bit 1) act
      -- Declare return value output
      newScope (portName "ret") (declareOut ret t)
declareOut (IfcTermProduct x0 x1) (IfcTypeProduct tx0 tx1) = do
  declareOut x0 tx0
  declareOut x1 tx1
declareOut (IfcTermFun fun) (IfcTypeFun argType retType) = do
  -- Declare argument as input
  argName <- getArgName
  arg <- newScope (portName argName) (declareIn argType)
  -- Apply argument and declare return value as output
  declareOut (fun arg) retType

-- Typed version of 'declareOut'
declareOutput :: Interface a => String -> a -> Declare ()
declareOutput str out =
  newScope (portName str) (declareOut (toIfcTerm out) (toIfcType out))

-- Declare an input, generically over any interface type
declareIn :: IfcType -> Declare IfcTerm
declareIn (IfcTypeField info t) =
  newScope info (declareIn t)
declareIn IfcTypeUnit = return IfcTermUnit
declareIn (IfcTypeBV w) = do
  bv <- declareInputBV "" w
  return (IfcTermBV bv)
declareIn (IfcTypeAction t) = do
  alwaysEn <- getAlwaysEnabled
  enName <- getMethodEnableName
  -- Declare return value as input
  ret <- newScope (portName "ret") (declareIn t)
  -- Is action always enabled?
  if alwaysEn
    then return (IfcTermAction (return ret))
    else do
      -- Create enable wire
      enWire :: Wire (Bit 1) <- liftModule (makeWire 0)
      -- Declare enable signal as output
      declareOutputBV enName (toBV (val enWire))
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
      drivers <- forM argTypes \argType -> do
        nm <- getArgName
        newScope (portName nm) (driver argType)
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
    driver (IfcTypeField info t) = newScope info (driver t)
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
  newScope (portName str) do
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
