{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Stmt
Description : Monadic wrapper for the Recipe language
Copyright   : (c) 2020 Matthew Naylor
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Stmt
  ( Stmt
  , tick
  , skip
  , action
  , wait
  , par
  , while
  , background
  , runStmt
  , runStmtOn
  ) where

-- Standard imports
import Prelude
import Control.Monad

-- Blarney imports
import Blarney.Core
import Blarney.Recipe

-- |Statement monad, with mondic bind for sequential composition.
data Stmt a = Stmt Recipe a

instance Monad Stmt where
  return a = Stmt Skip a
  Stmt r1 a >>= f = Stmt (Seq [r1, r2]) b
    where Stmt r2 b = f a

instance Functor Stmt where
  fmap = liftM

instance Applicative Stmt where
  pure = return
  (<*>) = ap

skip :: Stmt ()
skip = Stmt Skip ()

tick :: Stmt ()
tick = Stmt Tick ()

action :: Action () -> Stmt ()
action act = Stmt (Action act) ()

par :: [Stmt ()] -> Stmt ()
par stmts = Stmt (Par [r | Stmt r _ <- stmts]) ()

wait :: Bit 1 -> Stmt ()
wait c = Stmt (Wait c) ()

while :: Bit 1 -> Stmt () -> Stmt ()
while c (Stmt r _) = Stmt (While c r) ()

ifThenElseStmt :: Bit 1 -> Stmt () -> Stmt () -> Stmt ()
ifThenElseStmt c (Stmt r1 _) (Stmt r2 _) =
  Stmt (If c r1 r2) ()

instance IfThenElse (Bit 1) (Stmt ()) where
  ifThenElse = ifThenElseStmt

background :: Stmt () -> Stmt ()
background (Stmt r _) = Stmt (Background r) ()

-- |Run a statement with a start pulse that is high only on the first
-- cycle of execution, and ignore the finish pulse.
runStmt :: Stmt () -> Module ()
runStmt (Stmt r _) = runRecipe r

-- |Run a statement, triggered by the given start pulse.
-- Returns the finish pulse.
runStmtOn :: Bit 1 -> Stmt () -> Module (Bit 1)
runStmtOn pulse (Stmt r _) = runRecipeOn pulse r
