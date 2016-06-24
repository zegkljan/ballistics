module Ui.Bindings
    ( display
    , reshape
    , keyboardMouse
    ) where

import           Graphics.UI.GLUT
import           Ui.Display

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()
