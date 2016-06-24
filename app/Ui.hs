module Ui where

import           Graphics.UI.GLUT
import           Ui.Bindings

runUi :: IO ()
runUi = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  reshapeCallback $= Just reshape
  mainLoop
