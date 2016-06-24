module Ui.Display
    ( display
    ) where

import           Graphics.UI.GLUT
import           Ui.Cube

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  cube 0.2
  flush
