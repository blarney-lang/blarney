{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE NoDeriveAnyClass           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Blarney.Core.Module
Description : Blarney modules
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

We split the RTL monad into a Module monad and an Action monad, giving a
more familiar HDL structure in which modules instantiate other modules,
and express behaviour through 'always' blocks containing actions.
Actions cannot instantiate modules.
-}
module Blarney.Core.Module
  ( -- * Modules and actions
    Module(..), Action(..),

    -- * Lift actions to modules
    always,

    -- * Action of doing nothing
    noAction,

    -- * Conditional actions
    when, whenR, switch, (-->),

    -- * Validity of a value
    Valid(..),

    -- * Variable value (read)
    Val(..),

    -- * Variable assignment (write)
    Assign(..),

    -- * Naming hints
    withNameHint,
    withName,
    noName,

    -- * Registers
    Reg(..), makeReg, makeRegU, makeDReg,

    -- * Wires
    Wire(..), makeWire, makeWireU,
    TriStateWire(..), makeTriStateWire, makeTriStateWireU,

    -- * Read-Write and Write-Only interfaces
    ReadWrite(..), WriteOnly(..),

    -- * Register files
    RegFile(..), makeRegFileInit, makeRegFile,

    -- * Other actions
    finish, RTL.display, RTL.display_, assert, dynamicAssert, staticAssert,

    -- * External inputs and outputs
    input, inputBV, output, outputBV,

    -- * Add a 'BV' as a netlist root
    addRoots,

    -- * Run a pure module
    runPureModule
  ) where

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Prim
import Blarney.Core.FShow
import Blarney.Core.Prelude
import Blarney.Core.IfThenElse
import qualified Blarney.Core.RTL as RTL
import qualified Blarney.Core.JList as JL

-- Standard imports
import Prelude
import Data.IORef
import Data.Array
import GHC.TypeLits
import GHC.Generics
import Control.Monad.Fix
import Control.Monad hiding (when)

-- |A module is just a wrapper around the RTL monad
newtype Module a = M { runModule :: RTL.RTL a }
  deriving (Functor, Applicative, Monad, MonadFix)

-- |An action is just a wrapper around the RTL monad
newtype Action a = A { runAction :: RTL.RTL a }
  deriving (Functor, Applicative, Monad, MonadFix)

-- |Execute an action on every clock cycle
always :: Action a -> Module a
always a = M (runAction a)

-- |Action of doing nothing
noAction :: Action ()
noAction = return ()

-- | Set name hint
withNameHint :: NameHint -> Module a -> Module a
withNameHint hint m = M $ RTL.withNameHint hint (runModule m)

-- | Set name hint
withName :: String -> Module a -> Module a
withName nm m = withNameHint (NmRoot 10 nm) m

-- | Tell plugin not to generate name
noName :: Module a -> Module a
noName m = m

-- |Conditional block over actions
when :: Bit 1 -> Action () -> Action ()
when c a = A (RTL.when c (runAction a))

-- |Conditional block over actions with return value
whenR :: Bit 1 -> Action a -> Action a
whenR c a = A (RTL.whenR c (runAction a))

-- |If-then-else statement for actions
ifThenElseAction :: Bits a => Bit 1 -> Action a -> Action a -> Action a
ifThenElseAction c a b = A (RTL.ifThenElseRTL c (runAction a) (runAction b))

-- |Overloaded if-then-else
instance Bits a => IfThenElse (Bit 1) (Action a) where
  ifThenElse = ifThenElseAction

-- |Switch statement over actions
switch :: Bits a => a -> [(a, Action ())] -> Action ()
switch subject alts =
  A (RTL.switch subject [(lhs, runAction rhs) | (lhs, rhs) <- alts])

-- |Operator for switch statement alternatives
infixl 0 -->
(-->) :: a -> b -> (a, b)
lhs --> rhs = (lhs, rhs)

-- |Validity of a value
class Valid t where
  valid :: t -> Bit 1

-- |Variable value (read)
class Val v a | v -> a where
  val :: v -> a
-- |Variable assignment (write)
infix 1 <==
class Assign v where
  (<==) :: Bits a => v a -> a -> Action ()

-- |Register read and write
instance Val (RTL.Reg t) t where
  val reg = RTL.regVal reg
instance Assign RTL.Reg where
  v <== x = A (RTL.writeReg v x)

-- |Wire read and write
instance Val (RTL.Wire t) t where
  val wire = RTL.wireVal wire
instance Assign RTL.Wire where
  v <== x = A (RTL.writeWire v x)

-- | Blarney's register Module type
data Reg t = Reg { -- | read method
                   readReg :: t
                   -- | write method
                 , writeReg :: t -> Action ()
                 } deriving (Generic)
instance Val (Reg t) t where
  val reg = readReg reg
instance Assign Reg where
  reg <== x = writeReg reg x
fromRTLReg :: Bits t => RTL.Reg t -> Reg t
fromRTLReg r = Reg { readReg  = val r
                   , writeReg = \x -> r <== x
                   }
-- |Create register with initial value
makeReg :: Bits a => a -> Module (Reg a)
makeReg init = M (fromRTLReg <$> RTL.makeReg init)

-- |Create wire with don't care initial value
makeRegU :: Bits a => Module (Reg a)
makeRegU = M (fromRTLReg <$> RTL.makeRegU)

-- | Blarney's wire Module type
data Wire t = Wire { -- | read method
                     readWire :: t
                     -- | write method
                   , writeWire :: t -> Action ()
                     -- | active method
                   , active :: Bit 1
                   } deriving (Generic)
instance Val (Wire t) t where
  val wire = readWire wire
instance Assign Wire where
  wire <== x = writeWire wire x
fromRTLWire :: Bits t => RTL.Wire t -> Wire t
fromRTLWire w = Wire { readWire  = val w
                     , writeWire = \x -> w <== x
                     , active    = RTL.active w
                     }

-- | Create a wire with a default value
makeWire :: Bits a => a -> Module (Wire a)
makeWire dflt = M (fromRTLWire <$> RTL.makeWire dflt)

-- | Create a wire with a don't care default value
makeWireU :: Bits a => Module (Wire a)
makeWireU = M (fromRTLWire <$> RTL.makeWireU)

-- | A newtype to discriminate tristate wires from standard wires
newtype TriStateWire t = TriStateWire (Wire t) deriving (Generic)
instance Val (TriStateWire t) t where
  val (TriStateWire wire) = readWire wire
instance Assign TriStateWire where
  (TriStateWire wire) <== x = writeWire wire x

-- | Create a tristate wire with a default value
makeTriStateWire :: Bits a => a -> Module (TriStateWire a)
makeTriStateWire dflt =
  M (TriStateWire . fromRTLWire <$> RTL.makeTriStateWire dflt)

-- | Create a tristate wire with a don't care default value
makeTriStateWireU :: Bits a => Module (TriStateWire a)
makeTriStateWireU =
  M (TriStateWire . fromRTLWire <$> RTL.makeTriStateWireU)

-- |Read-Write interface
data ReadWrite a =
  ReadWrite {
    rwReadVal :: a,
    rwWriteVal :: a -> Action ()
  } deriving Generic

instance Val (ReadWrite t) t where
  val = rwReadVal
instance Assign ReadWrite where
  (<==) = rwWriteVal

-- |Write-Only interface
data WriteOnly a =
  WriteOnly {
    woWriteVal :: a -> Action ()
  } deriving Generic

instance Assign WriteOnly where
  (<==) = woWriteVal

-- |A DReg holds the assigned value only for one cycle.
-- At all other times, it has the given default value.
makeDReg :: Bits a => a -> Module (Reg a)
makeDReg defaultVal = M (fromRTLReg <$> RTL.makeDReg defaultVal)

-- |External input declaration
input :: KnownNat n => String -> PortId -> Module (Bit n)
input nm pid = M (RTL.input nm pid)

-- |External input declaration (untyped)
inputBV :: String -> Width -> PortId -> Module BV
inputBV nm w pid = M (RTL.inputBV nm w pid)

-- |External output declaration
output :: String -> PortId -> Bit n -> Module ()
output nm pid out = M (RTL.output nm pid out)

-- |External output declaration (untyped)
outputBV :: String -> PortId -> BV -> Module ()
outputBV nm pid bv = M (RTL.outputBV nm pid bv)

data RegFile a d =
  RegFile {
    index  :: a -> d
  , update :: a -> d -> Action ()
  }

toRegFile :: RTL.RegFileRTL a d -> RegFile a d
toRegFile rf = RegFile (RTL.lookupRTL rf) (\a d -> A (RTL.updateRTL rf a d))

-- |Create register file with initial contents
makeRegFileInit :: forall a d. (Bits a, Bits d) =>
                     String -> Module (RegFile a d)
makeRegFileInit initFile = M (liftM toRegFile $ RTL.makeRegFileInit initFile)

-- |Create uninitialised register file
makeRegFile :: forall a d. (Bits a, Bits d) => Module (RegFile a d)
makeRegFile = M (liftM toRegFile RTL.makeRegFile)

-- | Terminate simulator
finish :: Action ()
finish = A RTL.finish

-- | Assert that a predicate holds
assert :: Bit 1 -> String -> Action ()
assert pred msg = A $ RTL.assert pred msg

-- |Simulation-time assertion
dynamicAssert :: Bit 1 -> String -> Action ()
dynamicAssert cond str =
  when (inv cond) do
    RTL.display ("Assertion failed: " ++ str)
    finish

-- |Elaboration-time assertion
staticAssert :: Bool -> String -> Module ()
staticAssert cond str =
  if cond then return () else error ("Assertion failed: " ++ str)

-- |Display statement
instance a ~ () => RTL.Displayable (Action a) where
  disp x suffix = A (RTL.disp x suffix)

-- |Add roots
addRoots :: [BV] -> Module ()
addRoots roots = M (RTL.addRoots roots)

-- | Run a pure module, i.e. a module that has no side effects.
--   If the module has side effects, raise an error.
runPureModule :: Module a -> String -> a
runPureModule mod errStr = RTL.evalPureRTL (runModule mod) errStr
