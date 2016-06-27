module Terrain ( Terrain
               , RawTerrain
               , HeightMapTerrain
               , createHeightMapTerrain) where

import           Data.Vec            as V
import qualified Data.Vector.Unboxed as UV

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
               -> UV.Vector Vec3D

{-|
  A terrain type that directly implements the Terrain typeclas.

  It directly contains the implementation of the two class functions.
-}
data RawTerrain = RawTerrain { aboveFunction :: Vec3D -> Bool    -- ^ de facto implementation of the isAbove function of the Terrain typeclass
                             , triangles     :: UV.Vector Vec3D  -- ^ de facto implementation of the getTriangles function of the Terrain typeclass
                             }

instance Terrain RawTerrain where
  isAbove = aboveFunction
  getTriangles = triangles


{-|
  A terrain type that represents a terrain in a form of height map.

  The terrain is defined as a surface that is a function of the X and Y coordinates and the function value is the Z coordinate.
-}
data HeightMapTerrain = HeightMapTerrain (Vec3D -> Bool) (UV.Vector Vec3D)

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
createHeightMapTerrain (xlb, xub, xn) (ylb, yub, yn) hf = undefined
