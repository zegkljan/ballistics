module Main where

import           Terrain
import           Ui
import           Utils

main :: IO ()
main = do
  putStrLn $ "Ballistics version " ++ version ++ " running."
  runUi (createHeightMapTerrain (-100, 100, 200) (-100, 100, 200) (+))
