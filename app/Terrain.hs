{-# LANGUAGE FlexibleContexts #-}
module Terrain ( Terrain(..)
               , RawTerrain(..)
               , HeightMapTerrain(..)
               , createHeightMapTerrain
               ) where

import           Data.Array.Repa as R
import           Data.Vec        as V
import           Debug.Trace
import           Utils

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
               -> Array U DIM1 (Vec3D, Vec3D, Vec3D)

{-|
  A terrain type that directly implements the Terrain typeclas.

  It directly contains the implementation of the two class functions.
-}
data RawTerrain = RawTerrain { aboveFunction :: Vec3D -> Bool                      -- ^ de facto implementation of the isAbove function of the Terrain typeclass
                             , triangles     :: Array U DIM1 (Vec3D, Vec3D, Vec3D) -- ^ de facto implementation of the getTriangles function of the Terrain typeclass
                             }

instance Terrain RawTerrain where
  isAbove = aboveFunction
  getTriangles = triangles


{-|
  A terrain type that represents a terrain in a form of height map.

  The terrain is defined as a surface that is a function of the X and Y coordinates and the function value is the Z coordinate.
-}
data HeightMapTerrain = HeightMapTerrain (Vec3D -> Bool) (Array U DIM1 (Vec3D, Vec3D, Vec3D))

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
                                                              h = R.zipWith hf xx yy
                                                              abF v = V.get V.n0 v < V.get V.n2 v
                                                              trs = triangularize xx yy h
                                                           in HeightMapTerrain abF trs

{-|
  Converts grid points to an array of triangles.

  The three arguments to the function are X, Y and Z coordinates of the points
  of a grid. All the three arguments must have the same shape.
-}
triangularize :: (Source r1 Double, Source r2 Double, Source r3 Double)
              => Array r1 DIM2 Double -- ^ X coordinates of the points
              -> Array r2 DIM2 Double -- ^ Y coordinates of the points
              -> Array r3 DIM2 Double -- ^ Z coordinates of the points
              -> Array U DIM1 (Vec3D, Vec3D, Vec3D)
triangularize d1 d2 d3 | (listOfShape (extent d1) /= listOfShape (extent d2)) || (listOfShape (extent d2) /= listOfShape (extent d3)) = error "Arrays must have the same shape."
                       | rank (extent d1) /= 2 || rank (extent d2) /= 2 || rank (extent d3) /= 2 = error "The arrays must be 2-dimensional."
triangularize d1 d2 d3 = let (Z R.:. rs R.:. cs) = extent d1
                             trs = (rs - 1) * (cs - 1) * 2
                             tf f1 f2 f3 (Z R.:. r R.:. c R.:. 0) = ( Vec3D (f1 (ix2 r c))       (f2 (ix2 r c))       (f3 (ix2 r c))
                                                                    , Vec3D (f1 (ix2 r (c + 1))) (f2 (ix2 r (c + 1))) (f3 (ix2 r (c + 1)))
                                                                    , Vec3D (f1 (ix2 (r + 1) c)) (f2 (ix2 (r + 1) c)) (f3 (ix2 (r + 1) c)) )
                             tf f1 f2 f3 (Z R.:. r R.:. c R.:. 1) = ( Vec3D (f1 (ix2 r (c + 1)))       (f2 (ix2 r (c + 1)))       (f3 (ix2 r (c + 1)))
                                                                    , Vec3D (f1 (ix2 (r + 1) (c + 1))) (f2 (ix2 (r + 1) (c + 1))) (f3 (ix2 (r + 1) (c + 1)))
                                                                    , Vec3D (f1 (ix2 (r + 1) c))       (f2 (ix2 (r + 1) c))       (f3 (ix2 (r + 1) c)) )
                          in computeUnboxedS $ reshape (ix1 trs) $ traverse3 d1 d2 d3 (\_ _ _ -> (ix3 (rs - 1) (cs - 1) 2)) tf
