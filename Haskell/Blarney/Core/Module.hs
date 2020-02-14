{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
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
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

We split the RTL monad into a Module monad and an Action monad, giving a
more familar HDL structure in which modules instantiate other modules,
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

    -- * Registers
    Reg(..), makeReg, makeRegU, makeDReg,

    -- * Wires
    Wire(..), makeWire, makeWireU,

    -- * Read-Write and Write-Only interfaces
    ReadWrite(..), WriteOnly(..),

    -- * Register files
    RegFile(..), makeRegFileInit, makeRegFile,

    -- * Other actions
    finish, RTL.display, RTL.display_,

    -- * External inputs and outputs
    input, inputBV, output, outputBV,

    -- * Convert RTL to a netlist
    addRoots, netlist
  ) where

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Net
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Prim
import Blarney.Core.FShow
import Blarney.Core.Prelude
import Blarney.Core.IfThenElse
import qualified Blarney.Core.RTL as RTL

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

-- |Conditional block over actions
when :: Bit 1 -> Action () -> Action ()
when c a = A (RTL.when c (runAction a))

-- |Conditional block over actions with return value
whenR :: Bit 1 -> Action a -> Action a
whenR c a = A (RTL.whenR c (runAction a))

-- |If-then-else statement for actions
ifThenElseAction :: Bit 1 -> Action () -> Action () -> Action ()
ifThenElseAction c a b = A (RTL.ifThenElseRTL c (runAction a) (runAction b))

-- |Overloaded if-then-else
instance IfThenElse (Bit 1) (Action ()) where
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
  val :: Bits a => v -> a
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

-- |Create wire with default value
makeWire :: Bits a => a -> Module (Wire a)
makeWire init = M (fromRTLWire <$> RTL.makeWire init)

-- |Create wire with don't care default value
makeWireU :: Bits a => Module (Wire a)
makeWireU = M (fromRTLWire <$> RTL.makeWireU)

-- |Read-Write interface
data ReadWrite a =
  ReadWrite {
    rwReadVal :: a,
    rwWriteVal :: a -> Action ()
  }

instance Val (ReadWrite t) t where
  val = rwReadVal
instance Assign ReadWrite where
  (<==) = rwWriteVal

-- |Write-Only interface
data WriteOnly a =
  WriteOnly {
    woWriteVal :: a -> Action ()
  }

instance Assign WriteOnly where
  (<==) = woWriteVal

-- |A DReg holds the assigned value only for one cycle.
-- At all other times, it has the given default value.
makeDReg :: Bits a => a -> Module (Reg a)
makeDReg defaultVal = M (fromRTLReg <$> RTL.makeDReg defaultVal)

-- |External input declaration
input :: KnownNat n => String -> Module (Bit n)
input str = M (RTL.input str)

-- |External input declaration (untyped)
inputBV :: String -> Width -> Module BV
inputBV str w = M (RTL.inputBV str w)

-- |External output declaration
output :: String -> Bit n -> Module ()
output str out = M (RTL.output str out)

-- |External output declaration (untyped)
outputBV :: String -> BV -> Module ()
outputBV str bv = M (RTL.outputBV str bv)

data RegFile a d =
  RegFile {
    (!)    :: a -> d
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

-- |Terminate simulator
finish :: Action ()
finish = A RTL.finish

-- |Display statement
instance RTL.Displayable (Action a) where
  disp x suffix = A (RTL.disp x suffix)

-- |Add roots
addRoots :: [BV] -> Module ()
addRoots roots = M (RTL.addRoots roots)

-- |Convert module to a netlist
netlist :: Module () -> IO Netlist
netlist m = RTL.netlist (runModule m)
