-- 2D Mesh combinators

module Mesh where

import Prelude
import Data.List
import qualified Blarney as B

type Mesh a = [[a]]

on :: (a -> b) -> Mesh a -> Mesh b
on f m = map (map f) m

onM_ :: Monad m => (a -> m b) -> Mesh a -> m ()
onM_ f m = mapM_ (mapM_ f) m

merge :: (a -> b -> c) -> Mesh a -> Mesh b -> Mesh c
merge f = zipWith (zipWith f)

overlay :: Mesh [a] -> Mesh [a] -> Mesh [a]
overlay = merge (++)

neighbours :: Mesh a -> Mesh [a]
neighbours m = right m `overlay` left m `overlay` above m `overlay` below m

right :: Mesh a -> Mesh [a]
right m = [map (:[]) xs ++ [[]] | x:xs <- m]

left :: Mesh a -> Mesh [a]
left = map reverse . right . map reverse

above :: Mesh a -> Mesh [a]
above = transpose . left . transpose

below :: Mesh a -> Mesh [a]
below = transpose . right . transpose

trim :: Mesh a -> Mesh a
trim = map tail . map init . tail . init

mesh :: B.Bits a => (B.Reg a -> [a] -> B.Action ()) ->
          Mesh (B.Reg a) -> B.Action ()
mesh f m = uncurry f `onM_` trim (merge (,) m (map B.val `on` neighbours m))
