module Blarney.Action where

import Blarney.Bit

-- Write monad for collecting acts
data Action a = Action [Act] a

-- An act can be an assigment or a conditional
data Act =
    ActAssign VarId Pin
  | ActIfThenElse Pin [Act] [Act]

type VarId = Int

-- Action is a writer monad
instance Monad Action where
  return a = Action [] a
  m >>= f = case m of Action as a ->
                        case f a of Action bs b -> Action (as ++ bs) b

-- An empty action
empty :: Action ()
empty = Action [] ()

-- Add an act to the collection
addAct :: Act -> Action ()
addAct a = Action [a] ()

-- Extract the list of acts
runAction :: Action a -> [Act]
runAction (Action acts a) = acts
