{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-|
Module      : Blarney.Core.Interface
Description : Support for separate compilation
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

Rather than flattening the entire design hierarchy down to a single
netlist, we want to maintain the modular structure when generating
Verilog.  This module allows Blarney functions to be turned into
Verilog modules, and Verilog modules to be instantiated in a
Blarney description.
-}
module Blarney.Core.Interface
  ( Ifc           -- Monad for constructing Verilog interfaces
  , Interface(..) -- Class of types that can be converted to Verilog I/O ports
  , Modular(..)   -- Class of types that can be turned into Verilog modules
  , makeModule    -- Convert a Blarney function to a Verilog module
  , makeInstance  -- Instantiate a Verilog module in a Blarney description
  , makeInstanceWithParams  -- Allow synthesis-time Verilog parameters
  ) where

-- Standard imports
import Prelude
import GHC.TypeLits
import Control.Monad.Fix
import Control.Monad hiding (when)
import GHC.Generics hiding (R)

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Prim
import Blarney.Core.Module
import Blarney.Core.Prelude

-- The Ifc monad
-- =============

-- A reader/writer monad for creating modules and module instances,
-- with named inputs and outputs.

-- |The reader part provides component outputs or module inputs
type R = [(String, BV)]

-- |The writer part collects component/module inputs and outputs
data Pin = WritePin BV | ReadPin Integer
type W = [(String, Pin)]

-- |The 'Ifc' monad
newtype Ifc a = Ifc { runIfc :: R -> Module (W, a) }

instance Monad Ifc where
  return a = Ifc $ \r -> return ([], a)
  m >>= f = Ifc $ \r -> do
    (w0, a) <- runIfc m r
    (w1, b) <- runIfc (f a) r
    return (w0 ++ w1, b)

instance Applicative Ifc where
  pure = return
  (<*>) = ap

instance Functor Ifc where
  fmap = liftM

-- |Write a bit-vector to an interface pin
writePin :: String -> Bit n -> Ifc ()
writePin s a = Ifc $ \r -> return ([(s, WritePin (toBV a))], ())

-- |Read a bit-vector from an interface pin
readPin :: KnownNat n => String -> Ifc (Bit n)
readPin s = Ifc $ \r ->
  let out = case lookup s r of
              Nothing -> error ("readPin: unknown id '" ++ s ++ "'")
              Just x -> FromBV x
  in  return ([(s, ReadPin (natVal out))], out)

-- |Lift a Module computation to an Ifc computation
liftModule :: Module a -> Ifc a
liftModule m = Ifc $ \r -> do
  res <- m
  return ([], res)

-- |Create an instance of a module
instantiate :: String -> [Param] -> Ifc a -> Module a
instantiate name params ifc = do
    rec (w, a) <- runIfc ifc (custom w)
    addRoots [x | (s, WritePin x) <- w]
    return a
  where
    custom w =
      let inputs  = [(s, x) | (s, WritePin x) <- w]
          outputs = [(s, fromInteger n) | (s, ReadPin n) <- w]
          prim    = Custom name (map fst inputs) outputs params True
      in  zip (map fst outputs)
              (makePrim prim (map snd inputs) (map snd outputs))

-- |Create a module that can be instantiated
modularise :: Ifc a -> Module a
modularise ifc = mdo
    (w, a) <- runIfc ifc inps
    inps <- mod w
    return a
  where
    mod w = do
      let outputs = [(s, x) | (s, WritePin x) <- w]
      let inputs  = [(s, fromInteger n) | (s, ReadPin n) <- w]
      mapM (\(s,x) -> outputBV s x) outputs
      tmps <- mapM (\(s,n) -> inputBV s n) inputs
      return $ zip (map fst inputs) tmps

-- Interface class
-- ===============

-- |Any type in this class can be used as an input/output to a
-- module when doing separate compilation.
class Interface a where
  writePort :: String -> a -> Ifc ()
  readPort  :: String -> Ifc a

  -- For generic deriving
  default writePort :: (Generic a, GInterface (Rep a))
                    => String -> a -> Ifc ()
  default readPort :: (Generic a, GInterface (Rep a))
                   => String -> Ifc a
  writePort s x = gwritePort s "" (from x)
  readPort s = do { x <- greadPort s ""; return (to x) }

instance Interface () where
  writePort s _ = return ()
  readPort s = return ()

instance KnownNat n => Interface (Bit n) where
  writePort = writePin
  readPort = readPin

instance (Bits a, Interface a) => Interface (Action a) where
  writePort s act = do
    -- Get enable output
    enable <- readPort (s ++ "_en")
    -- Run action block when enabled
    res <- liftModule (always (whenR enable act))
    -- Feed action result back as input
    writePort (s ++ "_res") res

  readPort s = do
    -- Get result output
    res <- readPort (s ++ "_res")
    -- Create enable wire
    enable :: Wire (Bit 1) <- liftModule (makeWire 0)
    -- Put enable wire as input
    writePort (s ++ "_en") (val enable)
    -- When action block is called, trigger the enable line
    return (do { enable <== 1; return res })

-- Tuple instances
-- ===============

instance (Interface a, Interface b) => Interface (a, b) where
  writePort s ~(a, b) = do
    writePort (s ++ "_0") a
    writePort (s ++ "_1") b
  readPort s = do
    t0 <- readPort (s ++ "_0")
    t1 <- readPort (s ++ "_1")
    return (t0, t1)

instance (Interface a, Interface b, Interface c) => Interface (a, b, c) where
  writePort s ~(a, b, c) = do
    writePort (s ++ "_0") a
    writePort (s ++ "_1") b
    writePort (s ++ "_2") c
  readPort s = do
    t0 <- readPort (s ++ "_0")
    t1 <- readPort (s ++ "_1")
    t2 <- readPort (s ++ "_2")
    return (t0, t1, t2)

instance (Interface a, Interface b, Interface c, Interface d) =>
         Interface (a, b, c, d) where
  writePort s ~(a, b, c, d) = do
    writePort (s ++ "_0") a
    writePort (s ++ "_1") b
    writePort (s ++ "_2") c
    writePort (s ++ "_3") d
  readPort s = do
    t0 <- readPort (s ++ "_0")
    t1 <- readPort (s ++ "_1")
    t2 <- readPort (s ++ "_2")
    t3 <- readPort (s ++ "_3")
    return (t0, t1, t2, t3)

-- Function instances
-- ==================

instance (Bits a, Bits b, Interface a, Interface b) =>
         Interface (a -> Action b) where
  writePort s f = do
    -- Get function argument
    arg0 <- readPort (s ++ "_arg")
    -- Apply it
    writePort s (f arg0)

  readPort s = do
    -- Function argument is an input
    arg0 :: Wire a <- liftModule (makeWire dontCare)
    -- Supply it
    writePort (s ++ "_arg") (val arg0)
    -- Function result is an output
    act <- readPort s
    -- Return function that assigns argument
    return $ \a -> do
      arg0 <== a
      act

instance (Bits a, Bits b, Bits c,
          Interface a, Interface b, Interface c) =>
          Interface (a -> b -> Action c) where
  writePort s f = do
    writePort s (\(a, b) -> f a b)
  readPort s = do
    f <- readPort s
    return (\a b -> f (a, b))

instance (Bits a, Bits b, Bits c, Bits d,
          Interface a, Interface b, Interface c, Interface d) =>
          Interface (a -> b -> c -> Action d) where
  writePort s f = do
    writePort s (\(a, b, c) -> f a b c)
  readPort s = do
    f <- readPort s
    return (\a b c -> f (a, b, c))

instance (Bits a, Bits b, Bits c, Bits d, Bits e, Interface a,
          Interface b, Interface c, Interface d, Interface e) =>
          Interface (a -> b -> c -> d -> Action e) where
  writePort s f = do
    writePort s (\(a, b, c, d) -> f a b c d)
  readPort s = do
    f <- readPort s
    return (\a b c d -> f (a, b, c, d))

-- Generic deriving
-- ================

class GInterface f where
  gwritePort :: String -> String -> f a -> Ifc ()
  greadPort  :: String -> String -> Ifc (f a)

instance GInterface U1 where
  gwritePort s t ~U1 = return ()
  greadPort s t = return U1

instance (GInterface a, GInterface b) => GInterface (a :*: b) where
  gwritePort s t ~(x0 :*: x1) = do
    gwritePort s (t ++ "0") x0
    gwritePort s (t ++ "1") x1
  greadPort s t = do
    x0 <- greadPort s (t ++ "0")
    x1 <- greadPort s (t ++ "1")
    return (x0 :*: x1)

instance (GInterface a, Selector c) => GInterface (M1 S c a) where
  gwritePort s t ~(m@(M1 x)) =
    case null (selName m) of
      True -> gwritePort (s ++ "_" ++ t) "" x
      False -> gwritePort (s ++ "_" ++ selName m) "" x
  greadPort s t = do
    let ctr :: M1 S c a _ = undefined
    x <- case null (selName ctr) of
           True -> greadPort (s ++ "_" ++ t) ""
           False -> greadPort (s ++ "_" ++ selName ctr) ""
    return (M1 x)

instance GInterface a => GInterface (M1 D c a) where
  gwritePort s t ~(M1 x) = gwritePort s t x
  greadPort s t = do
    x <- greadPort s t
    return (M1 x)

instance GInterface a => GInterface (M1 C c a) where
  gwritePort s t ~(M1 x) = gwritePort s t x
  greadPort s t = do
    x <- greadPort s t
    return (M1 x)

instance Interface a => GInterface (K1 i a) where
  gwritePort s t ~(K1 x) = writePort s x
  greadPort s t = do
    x <- readPort s
    return (K1 x)

-- Standard instances
-- ==================

instance (Bits t, Interface t) => Interface (Reg t)
instance (Bits t, Interface t) => Interface (Wire t)

-- Modular class
-- =============

-- |Any type in this class can be turned into a module when doing
-- separate compilation.
class Modular a where
  makeMod :: a -> Module ()
  makeInst :: String -> [Param] -> a

instance Interface a => Modular (Module a) where
  makeMod m = modularise $ do
    a <- liftModule m
    writePort "out" a

  makeInst s ps = instantiate s ps $ do
    a <- readPort "out"
    return a

instance (Interface a, Interface b) => Modular (a -> Module b) where
  makeMod f = modularise $ do
    a <- readPort "in"
    b <- liftModule (f a)
    writePort "out" b

  makeInst s ps = \a ->
    instantiate s ps $ do
      writePort "in" a
      b <- readPort "out"
      return b

instance (Interface a, Interface b, Interface c) =>
         Modular (a -> b -> Module c) where
  makeMod f = makeMod (\(a, b) -> f a b)
  makeInst s ps = \a b -> makeInst s ps (a, b)

instance (Interface a, Interface b, Interface c, Interface d) =>
         Modular (a -> b -> c -> Module d) where
  makeMod f = makeMod (\(a, b, c) -> f a b c)
  makeInst s ps = \a b c -> makeInst s ps (a, b, c)

instance (Interface a, Interface b, Interface c, Interface d, Interface e) =>
         Modular (a -> b -> c -> d -> Module e) where
  makeMod f = makeMod (\(a, b, c, d) -> f a b c d)
  makeInst s ps = \a b c d -> makeInst s ps (a, b, c, d)

makeModule :: Modular a => a -> Module ()
makeModule = makeMod

makeInstance :: Modular a => String -> a
makeInstance s = makeInst s []

makeInstanceWithParams :: Modular a => String -> [Param] -> a
makeInstanceWithParams s ps = makeInst s ps
