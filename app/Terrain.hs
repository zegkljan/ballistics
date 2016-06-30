{-# LANGUAGE FlexibleContexts #-}
module Terrain ( Terrain
               , RawTerrain
               , HeightMapTerrain
               , createHeightMapTerrain
               , linspace
               , meshgrid) where

import           Data.Array.Repa as R
import           Data.Vec        as V

class Terrain a where
  {-|
    Determines whether a given point is "above" the terrain.
  -}
  isAbove :: a     -- ^ terrain
          -> Vec3D -- ^ location of the point
          -> Bool
  {-|
    Returns the triangles that form the actual terrain surface.
  -}
  getTriangles :: a -- ^ terrain
               -> Array U DIM3 Double

{-|
  A terrain type that directly implements the Terrain typeclas.

  It directly contains the implementation of the two class functions.
-}
data RawTerrain = RawTerrain { aboveFunction :: Vec3D -> Bool          -- ^ de facto implementation of the isAbove function of the Terrain typeclass
                             , triangles     :: Array U DIM3 Double  -- ^ de facto implementation of the getTriangles function of the Terrain typeclass
                             }

instance Terrain RawTerrain where
  isAbove = aboveFunction
  getTriangles = triangles


{-|
  A terrain type that represents a terrain in a form of height map.

  The terrain is defined as a surface that is a function of the X and Y coordinates and the function value is the Z coordinate.
-}
data HeightMapTerrain = HeightMapTerrain (Vec3D -> Bool) (Array U DIM3 Double)

instance Terrain HeightMapTerrain where
  isAbove (HeightMapTerrain f _) = f
  getTriangles (HeightMapTerrain _ t) = t

createHeightMapTerrain :: (Double, Double, Int) -- ^ lower bound, upper bound and number of grid points of the X coordinate
                       -> (Double, Double, Int) -- ^ lower bound, upper bound and number of grid points of the Y coordinate
                       -> (   Double
                           -> Double
                           -> Double
                          ) -- ^ height function
                       -> HeightMapTerrain
createHeightMapTerrain (xlb, xub, xn) (ylb, yub, yn) hf = let x = linspace xn xlb xub
                                                              y = linspace yn ylb yub
                                                              (xx, yy) = meshgrid x y
                                                              h = computeUnboxedS $ R.zipWith hf xx yy
                                                              abF v = V.get V.n0 v < V.get V.n2 v
                                                              trs = triangularize xx yy h
                                                           in HeightMapTerrain abF trs

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

triangularize :: Array U DIM2 Double
              -> Array U DIM2 Double
              -> Array U DIM2 Double
              -> Array U DIM3 Double
triangularize d1 d2 d3 | (listOfShape (extent d1) /= listOfShape (extent d2)) || (listOfShape (extent d2) /= listOfShape (extent d3)) = error "Arrays must have the same shape."
                       | rank (extent d1) /= 2 || rank (extent d2) /= 2 || rank (extent d3) /= 2 = error "The arrays must be 2-dimensional."
                       | ((listOfShape (extent d1) !! 1) /= 3) || ((listOfShape (extent d2) !! 1) /= 3) || ((listOfShape (extent d3) !! 1) /= 3) = error "The second dimension must have the size of 3."
triangularize d1 d2 d3 = let stacked = computeUnboxedS (d1 R.++ d2 R.++ d3)
