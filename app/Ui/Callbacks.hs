module Ui.Callbacks where

import           Control.Concurrent.STM        (TQueue, atomically, newTQueueIO,
                                                tryReadTQueue, writeTQueue)
import           Control.Exception
import           Control.Monad
import           Control.Monad.RWS.Strict      as RWS
import           Data.IORef
import           Graphics.Rendering.OpenGL     (($=))
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW              as GLFW
import           Prelude                       hiding (catch)

-- Copied from https://github.com/bsl/GLFW-b/blob/master/Graphics/UI/GLFW.hs so that we know what everything means.
-- type ErrorCallback           = Error -> String                                           -> IO ()
-- type WindowPosCallback       = Window -> Int -> Int                                      -> IO ()
-- type WindowSizeCallback      = Window -> Int -> Int                                      -> IO ()
-- type WindowCloseCallback     = Window                                                    -> IO ()
-- type WindowRefreshCallback   = Window                                                    -> IO ()
-- type WindowFocusCallback     = Window -> FocusState                                      -> IO ()
-- type WindowIconifyCallback   = Window -> IconifyState                                    -> IO ()
-- type FramebufferSizeCallback = Window -> Int -> Int                                      -> IO ()
-- type MouseButtonCallback     = Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
-- type CursorPosCallback       = Window -> Double -> Double                                -> IO ()
-- type CursorEnterCallback     = Window -> CursorState                                     -> IO ()
-- type ScrollCallback          = Window -> Double -> Double                                -> IO ()
-- type KeyCallback             = Window -> Key -> Int -> KeyState -> ModifierKeys          -> IO ()
-- type CharCallback            = Window -> Char                                            -> IO ()
-- type MonitorCallback         = Monitor -> MonitorState                                   -> IO ()

-- | Abstraction of various events.
data Evt =
    EvtErr             !GLFW.Error !String
  | EvtWindowSize      !GLFW.Window !Int !Int
  | EvtFramebufferSize !GLFW.Window !Int !Int
  | EvtKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EvtScroll          !GLFW.Window !Double !Double
  | EvtMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EvtCursorPos       !GLFW.Window !Double !Double

simpleErrCb :: GLFW.Error -> String -> IO ()
simpleErrCb e s = putStrLn . unwords $ [show e, s]

-- | Sets all callbacks.
setCallbacks :: TQueue Evt -> GLFW.Window -> IO ()
setCallbacks eq w = do
  GLFW.setErrorCallback             $ Just $ errCb eq
  GLFW.setWindowSizeCallback      w $ Just $ winSizeCb eq
  GLFW.setFramebufferSizeCallback w $ Just $ framebufSizeCb eq
  GLFW.setKeyCallback             w $ Just $ keyCb eq
  GLFW.setScrollCallback          w $ Just $ scrollCb eq
  GLFW.setMouseButtonCallback     w $ Just $ mouseButtonCb eq
  GLFW.setCursorPosCallback       w $ Just $ cursorPosCb eq

errCb          :: TQueue Evt -> GLFW.Error -> String -> IO ()
winSizeCb      :: TQueue Evt -> GLFW.Window -> Int -> Int -> IO ()
framebufSizeCb :: TQueue Evt -> GLFW.Window -> Int -> Int -> IO ()
keyCb          :: TQueue Evt -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
scrollCb       :: TQueue Evt -> GLFW.Window -> Double -> Double -> IO ()
mouseButtonCb  :: TQueue Evt -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCb    :: TQueue Evt -> GLFW.Window -> Double -> Double -> IO ()

errCb          eq e s            = atomically $ writeTQueue eq $ EvtErr e s
winSizeCb      eq win w h        = atomically $ writeTQueue eq $ EvtWindowSize win w h
framebufSizeCb eq win w h        = atomically $ writeTQueue eq $ EvtFramebufferSize win w h
keyCb          eq win k sc ks mk = atomically $ writeTQueue eq $ EvtKey win k sc ks mk
scrollCb       eq win x y        = atomically $ writeTQueue eq $ EvtScroll win x y
mouseButtonCb  eq win mb mbs mk  = atomically $ writeTQueue eq $ EvtMouseButton win mb mbs mk
cursorPosCb    eq win x y        = atomically $ writeTQueue eq $ EvtCursorPos win x y
