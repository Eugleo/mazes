module Main where

import BinaryTreeMaze

main :: IO ()
main = print "Haskell"

-- | Create a maze with binary tree algo with c columns and r rows
mkBTMaze :: Int -> Int -> IO ()
mkBTMaze c r = printGrid $ binaryTreeGrid c r
