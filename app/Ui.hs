module Ui where

import           Control.Concurrent.STM        (TQueue, atomically, newTQueueIO,
                                                tryReadTQueue, writeTQueue)
import           Control.Exception
import           Control.Monad
import           Control.Monad.RWS.Strict      as RWS
import           Data.IORef
import           Graphics.Rendering.OpenGL     (($=))
import qualified Graphics.Rendering.OpenGL     as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW              as GLFW
import           Prelude                       hiding (catch)
import           Terrain
import           Ui.Events
import           Ui.TerrainRendering
import           Ui.Utils
import           Utils

runUi :: (Terrain t) => t -> IO ()
runUi t = do
  -- initial window size
  let width  = 640
      height = 480

  -- create event queue
  evtQueue <- newTQueueIO :: IO (TQueue Evt)

  -- initialize GLFW system
  GLFW.setErrorCallback $ Just simpleErrCb
  r <- GLFW.init
  return $ assert r

  -- create window
  mw <- GLFW.createWindow width height ("Ballistics " ++ version) Nothing Nothing
  case mw of
    (Just w) -> do
      -- setup
      GLFW.makeContextCurrent mw
      setCallbacks evtQueue w
      GL.clearColor $= GL.Color4 0 0 0 (1 :: GL.GLfloat)
      GL.clearDepth $= (1 :: GL.GLdouble)
      GL.depthFunc $= Just GL.Lequal
      GL.hint GL.PerspectiveCorrection $= GL.Nicest

      -- initialize environment and state
      let env = Environment
            { envWindow = w
            , envEvtQueue = evtQueue
            , envTerrain = t
            }
          st = State
            { stateWindowWidth = width
            , stateWindowHeight = height
            , stateMouseDown = False
            , stateDragging = False
            , stateDragStartX = 0
            , stateDragStartY = 0
            , stateDragStartXAngle = 0
            , stateDragStartYAngle = 0
            , stateZDist = 10
            }

      -- start the main loop
      RWS.evalRWST (setWindow >> loop) env st

      -- deinitialize
      GLFW.setErrorCallback $ Just simpleErrCb
      GLFW.destroyWindow w
    Nothing -> putStrLn "Failed to create window"
  GLFW.terminate

loop :: App
loop = do
  -- get window from the environment
  win <- asks envWindow
  st <- get

  -- draw the content
  liftIO $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- payload
    drawCross 5

    GLFW.swapBuffers win
    GLFW.pollEvents
  processEvents

  -- determine whether to finish or to loop over again
  wc <- liftIO $ GLFW.windowShouldClose win
  unless wc loop

setWindow :: App
setWindow = do
  st <- get
  let w = stateWindowWidth st
      h = stateWindowHeight st
      zDist = stateZDist st
      pos = GL.Position 0 0
      size = GL.Size (fromIntegral w) (fromIntegral h)
      ratio = fromIntegral w / fromIntegral h :: Double
      zNear = 0.1 :: Double
      zFar = 200 + zDist :: Double
  liftIO $ do
    --putStrLn "Setting window parameters."
    GL.viewport $= (pos, size)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GLU.perspective 90 ratio zNear zFar
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    GL.translate $ GL.Vector3 0 0 (-zDist :: GL.GLdouble)

processEvents :: App
processEvents = do
  eq <- asks envEvtQueue
  me <- liftIO $ atomically $ tryReadTQueue eq
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

processEvent :: Evt -> App
processEvent e = case e of
  EvtErr e s -> do
    printEvent "error" [("error", show e), ("message", show s)]
    w <- asks envWindow
    liftIO $ GLFW.setWindowShouldClose w True
  EvtKey w k sc ks mk -> do
    printEvent "key" [("key", show k), ("scancode", show sc), ("key state", show ks), ("modifier keys", show mk)]
    when (ks == GLFW.KeyState'Pressed) $ do
      when (k == GLFW.Key'Escape) $ liftIO $ GLFW.setWindowShouldClose w True
  _ -> printEvent "other" []
