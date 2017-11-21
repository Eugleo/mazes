module Main where

import Draw

main :: IO ()
main = print "Haskell"

generateMazeWithDFS :: FilePath -> Int -> Int -> IO ()
generateMazeWithDFS file w h = genPng config file w h
  where config = Config 10 2 (10 * w + 4) (10 * h + 4)
