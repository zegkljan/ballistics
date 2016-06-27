module Ui where

import           Control.Concurrent        (threadDelay)
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Prelude                   hiding (catch)
import           Utils

runUi :: IO ()
runUi = do
  let width = 640
      height = 480
  initRes <- GLFW.init
  return $ assert initRes
  mw <- GLFW.createWindow width height ("Ballistics " ++ version) Nothing Nothing
  let win = case mw of
              (Just w) -> w
              Nothing -> error "Failed to create window"
  GLFW.makeContextCurrent mw
  GLFW.setKeyCallback win $ Just quitKeyCallback

  mainLoop win
  quit win

quitKeyCallback :: GLFW.KeyCallback
quitKeyCallback w key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Released)
                                        (GLFW.setWindowShouldClose w True)

quit :: GLFW.Window -> IO ()
quit w = GLFW.destroyWindow w >> GLFW.terminate

mainLoop :: GLFW.Window -> IO ()
mainLoop w = do
  GLFW.pollEvents
  wc <- GLFW.windowShouldClose w
  unless wc $ mainLoop w
