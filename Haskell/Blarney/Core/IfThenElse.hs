{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Core.IfThenElse
Description : Overloaded if-then-else
Copyright   : (c) Matthew Naylor, 2019, 2021
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Core.IfThenElse where

import Prelude

-- | Overloaded if-then-else
class IfThenElse b a where
  ifThenElse :: b -> a -> a -> a

instance IfThenElse Bool a where
  ifThenElse False a b = b
  ifThenElse True a b = a

-- | If-then-else chain
priorityIf :: IfThenElse cond ret => [(cond, ret)] -> ret
priorityIf [] = error "Blarney.Core.IfThenElse: priorityIf applied to []"
priorityIf ((a, b):rest) 
  | null rest = b
  | otherwise = if a then b else priorityIf rest

-- | If-then-else chain, with fallthrough case
priorityIfWithFallthrough :: IfThenElse cond ret => [(cond, ret)] -> ret -> ret
priorityIfWithFallthrough [] def = def
priorityIfWithFallthrough ((a, b):rest) def =
  if a then b else priorityIfWithFallthrough rest def
