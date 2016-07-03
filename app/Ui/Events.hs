module Ui.Events where

import           Control.Concurrent.STM        (TQueue, atomically, newTQueueIO,
                                                tryReadTQueue, writeTQueue)
import           Control.Exception
import           Control.Monad
import           Control.Monad.RWS.Strict      as RWS
import           Data.IORef
import           Data.List                     (intercalate)
import           Graphics.Rendering.OpenGL     (($=))
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW              as GLFW
import           Prelude                       hiding (catch)
import           Ui.Utils

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

simpleErrCb :: GLFW.Error -> String -> IO ()
simpleErrCb e s = putStrLn . unwords $ [show e, s]

-- | Sets all callbacks.
setCallbacks :: TQueue Evt -> GLFW.Window -> IO ()
setCallbacks eq w = do
  GLFW.setErrorCallback        $ Just $ errCb eq
  GLFW.setWindowSizeCallback w $ Just $ winSizeCb eq
  GLFW.setKeyCallback        w $ Just $ keyCb eq

errCb     :: TQueue Evt -> GLFW.Error -> String                                                 -> IO ()
winSizeCb :: TQueue Evt -> GLFW.Window -> Int -> Int                                            -> IO ()
keyCb     :: TQueue Evt -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()

errCb     eq e s            = atomically $ writeTQueue eq $ EvtErr e s
winSizeCb eq win w h        = atomically $ writeTQueue eq $ EvtWindowSize win w h
keyCb     eq win k sc ks mk = atomically $ writeTQueue eq $ EvtKey win k sc ks mk

printEvent :: String -> [(String, String)] -> App
printEvent n fields = liftIO $ putStrLn $ n ++ ": [" ++ intercalate ", " (map (\(l, s) -> l ++ ": " ++ s) fields) ++ "]"
