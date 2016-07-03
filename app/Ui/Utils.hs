{-# LANGUAGE GADTs      #-}
{-# LANGUAGE Rank2Types #-}
module Ui.Utils where

import           Control.Concurrent.STM    (TQueue)
import           Control.Monad.RWS.Strict  as RWS
import           Data.Vec                  as V
import           Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Terrain

-------------------
-- Useful datatypes
-------------------

-- | Abstraction of various events.
data Evt =
    EvtErr        !GLFW.Error !String
  | EvtWindowSize !GLFW.Window !Int !Int
  | EvtKey        !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys

-- | The application environment.
data Environment where
  Environment :: { envWindow   :: !GLFW.Window
                 , envEvtQueue :: TQueue Evt
                 , envTerrain  :: Terrain t => t } -> Environment

-- | The application state.
data State = State
  { stateWindowWidth     :: !Int
  , stateWindowHeight    :: !Int
  , stateMouseDown       :: !Bool
  , stateDragging        :: !Bool
  , stateDragStartX      :: !Double
  , stateDragStartY      :: !Double
  , stateDragStartXAngle :: !Double
  , stateDragStartYAngle :: !Double
  , stateZDist           :: !Double }

type App = RWST Environment () State IO ()

--------------------
-- Utility functions
--------------------

vec :: Vec3D -> Vertex3 Double
vec v = Vertex3 (V.get V.n0 v) (V.get V.n1 v) (V.get V.n2 v)

-- | Draws an arrow at the X axis given two points on that axis.
drawXArrow :: GLdouble -- ^ starting point of the arrow
           -> GLdouble -- ^ ending point of the arrow
           -> IO ()
drawXArrow xa xb = do
  renderPrimitive LineStrip $ do
    vertex $ Vertex3 xa 0 0
    vertex $ Vertex3 xb 0 0
  renderPrimitive LineStrip $ do
    vertex $ Vertex3 (xb - 0.2 * (xb - xa)) (negate 0.1 * (xb - xa)) 0
    vertex $ Vertex3 xb 0 0
    vertex $ Vertex3 (xb - 0.2 * (xb - xa)) (0.1 * (xb - xa)) 0
  renderPrimitive LineStrip $ do
    vertex $ Vertex3 (xb - 0.2 * (xb - xa)) 0 (negate 0.1 * (xb - xa))
    vertex $ Vertex3 xb 0 0
    vertex $ Vertex3 (xb - 0.2 * (xb - xa)) 0 (0.1 * (xb - xa))

{-|
  Draws a 3D cross of the given length.

  The cross are three arrows, each laying in a different axis, each intersecting
  at (0, 0, 0).
-}
drawCross :: GLdouble -- ^ size of the cross (length of the arrows)
          -> IO ()
drawCross l = do
  color $ Color3 1 0 (0 :: GLfloat)
  drawXArrow (negate l / 2) (l / 2)

  color $ Color3 0 1 (0 :: GLfloat)
  preservingMatrix $ do
    rotate 90 $ Vector3 0 0 (1 :: GLdouble)
    drawXArrow (negate l / 2) (l / 2)
  preservingMatrix $ do
    rotate (negate 90) $ Vector3 0 1 (0 :: GLdouble)
    drawXArrow (negate l / 2) (l / 2)
