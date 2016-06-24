module Main where

import           Ui

version :: String
version = "0.1.0.0"

main :: IO ()
main = do
  putStrLn $ "Ballistics version " ++ version ++ " running."
  runUi
