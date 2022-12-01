{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Core.IfThenElse
Description : Overloaded if-then-else and when conditionals
Copyright   : (c) Matthew Naylor, 2019, 2021
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Core.IfThenElse where

import Prelude
import Control.Monad qualified as M

-- | Overloaded if-then-else
class IfThenElse b a where
  ifThenElse :: b -> a -> a -> a

instance IfThenElse Bool a where
  ifThenElse False a b = b
  ifThenElse True a b = a

-- | If-then-else chain, with fallthrough case
priorityIf :: IfThenElse cond ret => [(cond, ret)] -> ret -> ret
priorityIf [] ft = ft
priorityIf ((a, b):rest) ft =
  if a then b else priorityIf rest ft

-- | Overloaded conditionals without an else part
class When cond act where
  when :: cond -> act () -> act ()

instance Applicative app => When Bool app where
  when cond body = M.when cond body
