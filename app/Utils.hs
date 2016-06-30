{-# LANGUAGE FlexibleInstances #-}
module Utils where

import Data.Array.Repa
import Data.Array.Repa.Algorithms.Matrix

import Text.PrettyPrint as PP
import Text.PrettyPrint.HughesPJClass as PPH


version :: String
version = "0.1.0.0"

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []



instance (Source t a, Pretty a) => Pretty (Array t DIM1 a) where
  pPrint a = brackets $ hcat $ punctuate (comma <> space) elems
    where elems = [ PPH.pPrint (a!j) | i <- [0..n-1], let j = Z :. i ]
          Z :. n = extent a

instance (Source t a, Pretty a) => Pretty (Array t DIM2 a) where
  pPrint a = vcat elems
    where elems = [ PPH.pPrint (slice a j) | i <- [0..n-1], let j = Any :. i :. All]
          Z :. n :. _m = extent a
