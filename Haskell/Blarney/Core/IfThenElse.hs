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

-- | If-then-else chain, with fallthrough case
priorityIf :: IfThenElse cond ret => [(cond, ret)] -> ret -> ret
priorityIf [] ft = ft
priorityIf ((a, b):rest) ft =
  if a then b else priorityIf rest ft
