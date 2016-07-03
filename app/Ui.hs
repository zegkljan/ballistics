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
import           Ui.Callbacks
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
      let env = Environment { envWindow = w
                            , envEvtQueue = evtQueue
                            , envTerrain = t
                            }
          st = State
            { stateWindowWidth = width
            , stateWindowHeight = height
            , stateMouseDown = False
            , stateMouseDragging = False
            , stateMouseX = 0
            , stateMouseY = 0
            , stateZDist = 10
            , stateAngleX = 0
            , stateAngleY = 0
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
  env <- ask
  let win = envWindow env
  st <- get

  -- draw the content
  liftIO $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- payload
    drawCross 5
    case env of
      Environment _ _ ter -> renderTerrain ter

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
      angleX = stateAngleX st
      angleY = stateAngleY st
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
    GL.rotate angleX $ GL.Vector3 1 0 (0 :: GL.GLdouble)
    GL.rotate angleY $ GL.Vector3 0 1 (0 :: GL.GLdouble)

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
  -- error: error message
  EvtErr e s -> do
    printEvent "error" [("error", show e), ("message", show s)]
    w <- asks envWindow
    liftIO $ GLFW.setWindowShouldClose w True
  -- key pressed: window key scancode key-state modifier-keys
  EvtKey w k sc ks mk -> do
    printEvent "key" [("key", show k), ("scancode", show sc), ("key state", show ks), ("modifier keys", show mk)]
    when (ks == GLFW.KeyState'Pressed) $
      when (k == GLFW.Key'Escape || k == GLFW.Key'Q) $ liftIO $ GLFW.setWindowShouldClose w True
  -- window size changed: window width height
  EvtWindowSize _ w h ->
    printEvent "window size" [("width", show w), ("height", show h)]
  -- framebuffer size changed: window width height
  EvtFramebufferSize _ w h -> do
    printEvent "frameuffer size" [("width", show w), ("height", show h)]
    modify $ \s -> s
      { stateWindowWidth = w
      , stateWindowHeight = h }
    setWindow
  -- scroll: window x-scroll y-scroll
  EvtScroll _ x y -> do
    printEvent "scroll" [("x", show x), ("y", show y)]
    modify $ \s -> let d = stateZDist s
                       dir = signum y
                    in s { stateZDist = d - 0.2 * dir * d}
    setWindow
  -- mouse button: window button button-state modifier-keys
  EvtMouseButton _ mb mbs mk -> do
    printEvent "mouse button" [("button", show mb), ("button state", show mbs), ("modifier keys", show mk)]
    when (mb == GLFW.MouseButton'1) $
      if mbs == GLFW.MouseButtonState'Pressed
      then modify $ \s -> s { stateMouseDown = True }
      else modify $ \s -> s { stateMouseDown = False
                            , stateMouseDragging = False }
  -- cursor position: window x y
  EvtCursorPos _ x y -> do
    printEvent "cursor position" [("x", show x), ("y", show y)]
    st <- get
    when (stateMouseDown st) $
      if not $ stateMouseDragging st
      then put $ st { stateMouseDragging = True
                     , stateMouseX = x
                     , stateMouseY = y }
      else do
        let dx = x - stateMouseX st
            dy = y - stateMouseY st
        modify $ \s -> s
          { stateAngleX = clip (negate 90) 90 $ stateAngleX s + 0.5 * dy
          , stateAngleY = 0.5 * dx + stateAngleY s
          , stateMouseX = x
          , stateMouseY = y }
        st2 <- get
        setWindow
  -- otherwise
  _ -> printEvent "other" []
