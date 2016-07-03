module Ui.TerrainRendering where

import           Data.Array.Repa           as R
import           Graphics.Rendering.OpenGL (($=))
import           Graphics.Rendering.OpenGL as GL
import           Terrain
import           Ui.Utils

renderTerrain :: (Terrain t) => t -> IO ()
renderTerrain t = renderPrimitive Triangles $ do
  let (t1, t2, t3) = getTriangles t R.! (ix1 0)
  GL.color $ GL.Color3 1 0 (0 :: GL.GLdouble)
  vertex (vec t1)
  GL.color $ GL.Color3 0 1 (0 :: GL.GLdouble)
  vertex (vec t2)
  GL.color $ GL.Color3 0 0 (1 :: GL.GLdouble)
  vertex (vec t3)
