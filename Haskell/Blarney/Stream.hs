module Blarney.Stream where

import Blarney
import Blarney.Queue
import Blarney.RAM

data Get a =
  Get {
    get :: RTL ()
  , canGet :: Bit 1
  , value :: a
  }

type Stream a = Get a

-- Convert a queue to a stream
toStream :: Queue a -> Stream a
toStream q =
  Get {
    get = q.deq
  , canGet = q.canDeq
  , value = q.first
  }

-- Map a function over a stream
instance Functor Get where
  fmap f stream = stream { value = f (stream.value) }

-- Create a stream from a seed
unfoldS :: (Bits s, Bits a)
        => (s -> Bit 1)
        -> (s -> a)
        -> (s -> s)
        -> Stream s
        -> RTL (Stream a)
unfoldS done next step seed = do
  -- Output buffer
  buffer :: Queue a <- makeQueue

  -- State
  state :: Reg s <- makeReg

  -- Currently active?
  active :: Reg (Bit 1) <- makeRegInit 0

  -- Start as soon as seed available
  when (active.val.inv .&. seed.canGet) $ do
    seed.get
    state <== seed.value
    active <== 1

  -- Generate output data
  when (active.val .&. buffer.notFull) $ do
    if state.val.done
      then active <== 0
      else do
        enq buffer (state.val.next)
        state <== state.val.step

  return (buffer.toStream)

-- Reduce a stream using an accumulator
foldS :: (Bits s, Bits a)
      => (s -> a -> Bit 1)
      -> (s -> a -> s)
      -> Stream s
      -> Stream a
      -> RTL (Stream s)
foldS done accum zero input = do
  -- Output buffer
  buffer :: Queue a <- makeQueue

  -- State
  state :: Reg s <- makeReg

  -- Currently active?
  active :: Reg (Bit 1) <- makeRegInit 0

  -- Start as soon initial accumulator is available
  when (active.val.inv .&. zero.canGet) $ do
    zero.get
    state <== zero.value
    active <== 1

  -- Generate output data
  when (active.val) $ do
    if done (state.val) (input.value)
      then when (buffer.notFull) $ do
             active <== 0
             enq buffer (state.val)
      else when (input.canGet) $ do
             state <== accum (state.val) (input.value)
             input.get

  return (toStream buffer)

-- Enumerate range
enumS :: (Bits a, Num a, _)
      => Stream (a, a)
      -> RTL (Stream a)
enumS = unfoldS done next step
  where
    done (from, to) = from .==. to
    next (from, to) = from
    step (from, to) = (from+1, to)

-- Stream data from a block RAM
-- Takes a stream of addresses and gives a stream of address/value pairs
loadS :: (Bits a, Bits b, Bits i)
       => RAM a b
       -> (i -> a)
       -> Stream i
       -> RTL (Stream (i, b))
loadS mem getAddr ins = do
  -- Inflight requests
  inflight :: Queue a <- makePipelineQueue 2

  -- Response buffer
  resps :: Queue b <- makePipelineQueue 2

  -- High when data is ready on the output of the bloack RAM
  ready :: Reg (Bit 1) <- makeDReg 0

  -- Give way to any writers of the RAM
  when (mem.writeEn.inv .&. ins.canGet .&. inflight.notFull) $ do
    load mem (ins.value.getAddr)
    enq inflight (ins.value)
    ins.get
    ready <== 1

  when (ready.val) $ do
    enq resps (mem.out)

  return $
    Get {
      canGet = resps.canDeq
    , get    = do { resps.deq; inflight.deq; }
    , value  = (inflight.first, resps.first)
    }

mapS :: (Bits a, Bits b) => (a -> b) -> Stream a -> RTL (Stream b)
mapS f xs = do
  -- Output buffer
  buffer :: Queue b <- makeQueue

  when (xs.canGet .&. buffer.notFull) $ do
    xs.get
    enq buffer (f (xs.value))

  return (toStream buffer)

filterS :: Bits a => (a -> Bit 1) -> Stream a -> RTL (Stream a)
filterS p xs = do
  -- Output buffer
  buffer :: Queue a <- makeQueue

  when (xs.canGet) $ do
    if xs.value.p
      then do
        when (buffer.notFull) $ do
          xs.get
          enq buffer (xs.value)
      else do
        xs.get

  return (toStream buffer)

zipWithS :: (Bits a, Bits b, Bits c)
         => (a -> b -> c)
         -> Stream a -> Stream b
         -> RTL (Stream c)
zipWithS f xs ys = do
  -- Output buffer
  buffer :: Queue c <- makeQueue

  when (xs.canGet .&. ys.canGet .&. buffer.notFull) $ do
    xs.get
    ys.get
    enq buffer (f (xs.value) (ys.value))

  return (toStream buffer)
