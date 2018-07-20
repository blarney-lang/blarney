{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DataKinds            #-} 
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ConstraintKinds      #-} 
{-# LANGUAGE ScopedTypeVariables  #-} 
{-# LANGUAGE RecursiveDo          #-} 

module Blarney.Interface where

import GHC.TypeLits
import GHC.Generics
import Blarney.Bit
import Blarney.Unbit
import Blarney.Bits
import Blarney.Prelude
import Blarney.RTL
import Control.Monad hiding (when)
import Control.Monad.Fix
import Prelude

-- The 'instantiation' monad
-- =========================

-- This monad is a reader/writer monad for creating an instance
-- of a custom component with named inputs and outputs.

-- The reader part provides component outputs
type InstR = [(String, Unbit)]

-- The writer part collects component inputs and outputs
data Pin = PinIn Unbit | PinOut Integer
type InstW = [(String, Pin)]

-- The 'Inst' monad
newtype Inst a = Inst { runInst :: InstR -> RTL (InstW, a) }

instance Monad Inst where
  return a = Inst $ \r -> return ([], a)
  m >>= f = Inst $ \r -> do
    (w0, a) <- runInst m r
    (w1, b) <- runInst (f a) r
    return (w0 ++ w1, b)

instance Applicative Inst where
  pure = return
  (<*>) = ap

instance Functor Inst where
  fmap = liftM

instInput :: String -> Bit n -> Inst ()
instInput s a = Inst $ \r -> return ([(s, PinIn (unbit a))], ())

instOutput :: KnownNat n => String -> Inst (Bit n)
instOutput s = Inst $ \r ->
  let out = case lookup s r of
              Nothing -> error ("instOutput: unknown output '" ++ s ++ "'")
              Just x -> Bit x
  in  return ([(s, PinOut (natVal out))], out)

liftRTL :: RTL a -> Inst a
liftRTL rtl = Inst $ \r -> do
  res <- rtl
  return ([], res)

lowerInst :: String -> [Param] -> Inst a -> RTL a
lowerInst name params inst = do
    rec (w, a) <- runInst inst (custom w)
    return a
  where
    custom w =
      let inputs  = [(s, x) | (s, PinIn x) <- w]
          outputs = [(s, fromInteger n) | (s, PinOut n) <- w]
          prim    = Custom name (map fst inputs) outputs params
      in  zip (map fst outputs)
              (makePrim prim (map snd inputs) (map snd outputs))

-- Interface class
-- ===============
--
-- Any type in this class can be used as an input/output to a
-- module when doing modular compilation.

class Interface a where
  makeInput  :: String -> RTL a
  makeOutput :: String -> a -> RTL ()
  putInput   :: String -> a -> Inst ()
  getOutput  :: String -> Inst a

  -- For generic deriving
  default makeInput :: (Generic a, GInterface (Rep a)) => String -> RTL a
  default makeOutput :: (Generic a, GInterface (Rep a)) => String -> a -> RTL ()
  default putInput :: (Generic a, GInterface (Rep a)) => String -> a -> Inst ()
  default getOutput :: (Generic a, GInterface (Rep a)) => String -> Inst a
  makeInput s = do { x <- gmakeInput s ""; return (to x) }
  makeOutput s x = gmakeOutput s "" (from x)
  putInput s x = gputInput s "" (from x)
  getOutput s = do { x <- ggetOutput s ""; return (to x) }

instance Interface () where
  makeInput s = return ()
  makeOutput s _ = return ()
  putInput s _ = return ()
  getOutput s = return ()

instance KnownNat n => Interface (Bit n) where
  makeInput = input
  makeOutput = output
  putInput = instInput
  getOutput = instOutput

instance (Bits a, Interface a) => Interface (RTL a) where
  makeInput s = do
    -- Create enable wire for RTL block
    enable :: Wire (Bit 1) <- makeWireDefault 0
    -- Enable wire is an output
    output (s ++ "_en") (val enable)
    -- Result wire is an input
    res <- makeInput (s ++ "_res") 
    -- When RTL block is called, trigger the enable output
    return (do { enable <== 1; return res })

  makeOutput s rtl = do
    -- Create result wire for RTL block
    result :: Wire a <- makeWire
    -- Result wire is an output
    makeOutput (s ++ "_res") (val result)
    -- Enable wire is an input
    enable :: Bit 1 <- input (s ++ "_en")
    -- Execute RTL block when enable triggered
    when (enable) $ do
      r <- rtl
      result <== r

  putInput s rtl = do
    -- Get enable output
    enable <- getOutput (s ++ "_en") 
    -- Run RTL block when enabled
    res <- liftRTL (whenR enable rtl)
    -- Feed RTL result back as input
    putInput (s ++ "_res") res

  getOutput s = do
    -- Get result output
    res <- getOutput (s ++ "_res")
    -- Create enable wire
    enable :: Wire (Bit 1) <- liftRTL (makeWireDefault 0)
    -- Put enable wire as input
    putInput (s ++ "_en") (val enable)
    -- When RTL block is called, trigger the enable line
    return (do { enable <== 1; return res })

-- Tuple instances
-- ===============

instance (Interface a, Interface b) => Interface (a, b) where
  makeInput s = do
    t0 <- makeInput (s ++ "_0")
    t1 <- makeInput (s ++ "_1")
    return (t0, t1)
  makeOutput s (a, b) = do
    makeOutput (s ++ "_0") a
    makeOutput (s ++ "_1") b
  putInput s (a, b) = do
    putInput (s ++ "_0") a
    putInput (s ++ "_1") b
  getOutput s = do
    t0 <- getOutput (s ++ "_0")
    t1 <- getOutput (s ++ "_1")
    return (t0, t1)

instance (Interface a, Interface b, Interface c) => Interface (a, b, c) where
  makeInput s = do
    t0 <- makeInput (s ++ "_0")
    t1 <- makeInput (s ++ "_1")
    t2 <- makeInput (s ++ "_2")
    return (t0, t1, t2)
  makeOutput s (a, b, c) = do
    makeOutput (s ++ "_0") a
    makeOutput (s ++ "_1") b
    makeOutput (s ++ "_2") c
  putInput s (a, b, c) = do
    putInput (s ++ "_0") a
    putInput (s ++ "_1") b
    putInput (s ++ "_2") c
  getOutput s = do
    t0 <- getOutput (s ++ "_0")
    t1 <- getOutput (s ++ "_1")
    t2 <- getOutput (s ++ "_2")
    return (t0, t1, t2)

instance (Interface a, Interface b, Interface c, Interface d) =>
         Interface (a, b, c, d) where
  makeInput s = do
    t0 <- makeInput (s ++ "_0")
    t1 <- makeInput (s ++ "_1")
    t2 <- makeInput (s ++ "_2")
    t3 <- makeInput (s ++ "_3")
    return (t0, t1, t2, t3)
  makeOutput s (a, b, c, d) = do
    makeOutput (s ++ "_0") a
    makeOutput (s ++ "_1") b
    makeOutput (s ++ "_2") c
    makeOutput (s ++ "_3") d
  putInput s (a, b, c, d) = do
    putInput (s ++ "_0") a
    putInput (s ++ "_1") b
    putInput (s ++ "_2") c
    putInput (s ++ "_3") d
  getOutput s = do
    t0 <- getOutput (s ++ "_0")
    t1 <- getOutput (s ++ "_1")
    t2 <- getOutput (s ++ "_2")
    t3 <- getOutput (s ++ "_3")
    return (t0, t1, t2, t3)

-- Function instances
-- ==================

instance (Bits a, Bits b, Interface a, Interface b) =>
         Interface (a -> RTL b) where
  makeInput s = do
    -- Function argument
    arg0 :: Wire a <- makeWire
    -- Function argument is an output
    makeOutput (s ++ "_arg") (val arg0)
    -- Function result is an input
    rtl <- makeInput s
    -- Return function that assigns argument
    return $ \a -> do
      arg0 <== a
      rtl

  makeOutput s f = do
    -- Function argument is an input
    arg0 <- makeInput (s ++ "_arg")
    -- Function result in an output
    makeOutput s (f arg0)

  putInput s f = do
    -- Get function argument
    arg0 <- getOutput (s ++ "_arg")
    -- Apply it
    putInput s (f arg0)

  getOutput s = do
    -- Function argument is an input
    arg0 :: Wire a <- liftRTL makeWire
    -- Supply it
    putInput (s ++ "_arg") (val arg0)
    -- Function result is an output
    rtl <- getOutput s
    -- Return function that assigns argument
    return $ \a -> do
      arg0 <== a
      rtl

instance (Bits a, Bits b, Bits c,
          Interface a, Interface b, Interface c) =>
          Interface (a -> b -> RTL c) where
  makeInput s = do
    f <- makeInput s
    return (\a b -> f (a, b))
  makeOutput s f = do
    makeOutput s (\(a, b) -> f a b)
  putInput s f = do
    putInput s (\(a, b) -> f a b)
  getOutput s = do
    f <- getOutput s
    return (\a b -> f (a, b))

instance (Bits a, Bits b, Bits c, Bits d,
          Interface a, Interface b, Interface c, Interface d) =>
          Interface (a -> b -> c -> RTL d) where
  makeInput s = do
    f <- makeInput s
    return (\a b c -> f (a, b, c))
  makeOutput s f = do
    makeOutput s (\(a, b, c) -> f a b c)
  putInput s f = do
    putInput s (\(a, b, c) -> f a b c)
  getOutput s = do
    f <- getOutput s
    return (\a b c -> f (a, b, c))

instance (Bits a, Bits b, Bits c, Bits d, Bits e, Interface a,
          Interface b, Interface c, Interface d, Interface e) =>
          Interface (a -> b -> c -> d -> RTL e) where
  makeInput s = do
    f <- makeInput s
    return (\a b c d -> f (a, b, c, d))
  makeOutput s f = do
    makeOutput s (\(a, b, c, d) -> f a b c d)
  putInput s f = do
    putInput s (\(a, b, c, d) -> f a b c d)
  getOutput s = do
    f <- getOutput s
    return (\a b c d -> f (a, b, c, d))

-- Generic deriving
-- ================

class GInterface f where
  gmakeInput  :: String -> String -> RTL (f a)
  gmakeOutput :: String -> String -> f a -> RTL ()
  gputInput   :: String -> String -> f a -> Inst ()
  ggetOutput  :: String -> String -> Inst (f a)

instance GInterface U1 where
  gmakeInput s t = return U1
  gmakeOutput s t U1 = return ()
  gputInput s t U1 = return ()
  ggetOutput s t = return U1

instance (GInterface a, GInterface b) => GInterface (a :*: b) where
  gmakeInput s t = do
    x0 <- gmakeInput s (t ++ "0")
    x1 <- gmakeInput s (t ++ "1")
    return (x0 :*: x1)
  gmakeOutput s t (x0 :*: x1) = do
    gmakeOutput s (t ++ "0") x0
    gmakeOutput s (t ++ "1") x1
  gputInput s t (x0 :*: x1) = do
    gputInput s (t ++ "0") x0
    gputInput s (t ++ "1") x1
  ggetOutput s t = do
    x0 <- ggetOutput s (t ++ "0")
    x1 <- ggetOutput s (t ++ "1")
    return (x0 :*: x1)

instance (GInterface a, Selector c) => GInterface (M1 S c a) where
  gmakeInput s t = do
    let ctr :: M1 S c a _ = undefined
    x <- case null (selName ctr) of
           True -> gmakeInput (s ++ "_" ++ t) ""
           False -> gmakeInput (s ++ "_" ++ selName ctr) ""
    return (M1 x)
  gmakeOutput s t m@(M1 x) = 
    case null (selName m) of
      True -> gmakeOutput (s ++ "_" ++ t) "" x
      False -> gmakeOutput (s ++ "_" ++ selName m) "" x
  gputInput s t m@(M1 x) = 
    case null (selName m) of
      True -> gputInput (s ++ "_" ++ t) "" x
      False -> gputInput (s ++ "_" ++ selName m) "" x
  ggetOutput s t = do
    let ctr :: M1 S c a _ = undefined
    x <- case null (selName ctr) of
           True -> ggetOutput (s ++ "_" ++ t) ""
           False -> ggetOutput (s ++ "_" ++ selName ctr) ""
    return (M1 x)

instance GInterface a => GInterface (M1 D c a) where
  gmakeInput s t = do
    x <- gmakeInput s t
    return (M1 x)
  gmakeOutput s t (M1 x) = gmakeOutput s t x
  gputInput s t (M1 x) = gputInput s t x
  ggetOutput s t = do
    x <- ggetOutput s t
    return (M1 x)

instance GInterface a => GInterface (M1 C c a) where
  gmakeInput s t = do
    x <- gmakeInput s t
    return (M1 x)
  gmakeOutput s t (M1 x) = gmakeOutput s t x
  gputInput s t (M1 x) = gputInput s t x
  ggetOutput s t = do
    x <- ggetOutput s t
    return (M1 x)

instance Interface a => GInterface (K1 i a) where
  gmakeInput s t = do
    x <- makeInput s
    return (K1 x)
  gmakeOutput s t (K1 x) = makeOutput s x
  gputInput s t (K1 x) = putInput s x
  ggetOutput s t = do
    x <- getOutput s
    return (K1 x)

-- Module class
-- ============
--
-- Any type in this class can be turned into a module when doing
-- modular compilation.

class Module a where
  makeMod :: a -> RTL ()
  makeInst :: String -> [Param] -> a

instance Interface a => Module (RTL a) where
  makeMod rtl = do
    a <- rtl
    makeOutput "out" a

  makeInst s ps = lowerInst s ps $ do
    a <- getOutput "out"
    return a

instance (Interface a, Interface b) => Module (a -> RTL b) where
  makeMod f = do
    a <- makeInput "in"
    b <- f a
    makeOutput "out" b

  makeInst s ps = \a ->
    lowerInst s ps $ do
      putInput "in" a
      a <- getOutput "out"
      return a

instance (Interface a, Interface b, Interface c) =>
         Module (a -> b -> RTL c) where
  makeMod f = makeMod (\(a, b) -> f a b)
  makeInst s ps = \a b -> makeInst s ps (a, b)

instance (Interface a, Interface b, Interface c, Interface d) =>
         Module (a -> b -> c -> RTL d) where
  makeMod f = makeMod (\(a, b, c) -> f a b c)
  makeInst s ps = \a b c -> makeInst s ps (a, b, c)

instance (Interface a, Interface b, Interface c, Interface d, Interface e) =>
         Module (a -> b -> c -> d -> RTL e) where
  makeMod f = makeMod (\(a, b, c, d) -> f a b c d)
  makeInst s ps = \a b c d -> makeInst s ps (a, b, c, d)

makeModule :: Module a => a -> RTL ()
makeModule = makeMod

makeInstance :: Module a => String -> a
makeInstance s = makeInst s []

makeInstanceWithParams :: Module a => String -> [Param] -> a
makeInstanceWithParams s ps = makeInst s ps
