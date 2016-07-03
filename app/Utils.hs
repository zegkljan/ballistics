{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Utils where

import           Data.Array.Repa                   as R
import           Data.Array.Repa.Algorithms.Matrix
import           Data.Vec                          as V
import           Data.Vector.Unboxed.Base          (Unbox)
import           Data.Vector.Unboxed.Deriving
import           Text.PrettyPrint                  as PP
import           Text.PrettyPrint.HughesPJClass    as PPH

derivingUnbox "Vec3D"
  [t| Vec3D -> (Double, Double, Double) |]
  [| \ v -> (get n0 v, get n1 v, get n2 v) |]
  [| \ (a, b, c) -> Vec3D a b c |]


version :: String
version = "0.1.0.0"

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

instance (Source t a, Pretty a) => Pretty (Array t DIM1 a) where
  pPrint a = brackets $ hcat $ punctuate (comma <> space) elems
    where elems = [ PPH.pPrint (a R.! j) | i <- [0..n-1], let j = Z R.:. i ]
          Z R.:. n = extent a

instance (Source t a, Pretty a) => Pretty (Array t DIM2 a) where
  pPrint a = vcat elems
    where elems = [ PPH.pPrint (R.slice a j) | i <- [0..n-1], let j = Any R.:. i R.:. All]
          Z R.:. n R.:. _m = extent a

{-|
  Similar to numpy's or MATLAB's linspace.

  Generates a vector of linearly spaced numbers in the given range and of the given number of elements.
  Due to the nature of floating point numbers, the last element may be slightly bigger than the upper bound.
-}
linspace :: Int    -- ^ the number of elements
         -> Double -- ^ lower bound
         -> Double -- ^ upper bound
         -> Array U DIM1 Double
linspace n lb ub | lb >= ub  = error "Lower bound must be strictly smaller than upper bound."
                 | otherwise = let d  = (ub - lb) / fromIntegral (n - 1)
                                in fromListUnboxed (ix1 n) [lb + d * fromIntegral k | k <- [0..n - 1]]

meshgrid :: Array U DIM1 Double
         -> Array U DIM1 Double
         -> (Array U DIM2 Double, Array U DIM2 Double)
meshgrid xs ys = let (Z R.:. lx) = extent xs
                     (Z R.:. ly) = extent ys
                     mgx = extend (Any R.:. (ly :: Int) R.:. All) xs
                     mgy = R.transpose $ extend (Any R.:. (lx :: Int) R.:. All) ys
                  in (computeUnboxedS mgx, computeUnboxedS mgy)
