module Main where

import           Ui
import           Utils

main :: IO ()
main = do
  putStrLn $ "Ballistics version " ++ version ++ " running."
  runUi
