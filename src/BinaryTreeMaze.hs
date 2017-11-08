module BinaryTreeMaze where

import Control.Monad
import Grid
import System.Random

-- | Return Cell with either the nothern or the eastern border Open
cellR :: IO Cell
cellR = randomRIO (0, 1) >>= return . (cells !!)
  where
    cells = [Cell Open Closed, Cell Closed Open]

-- | Return a grid, whose paths represend a BT
binaryTreeGrid :: Int -> Int -> IO Grid
binaryTreeGrid c r = body
  where
    firstRow = replicate (c - 1) (Cell Closed Open) ++ [Cell Closed Closed]
    -- first row needs to have all nothern borders closed
    body = (firstRow :) <$> (replicateM (r - 1) row)
    row = (++ [Cell Open Closed]) <$> (replicateM (c - 1) cellR)
    -- every row needs to have the eastern border of the rightmost cell closed

-- | Convert the Grid to a Maze and print it
printGrid :: IO Grid -> IO ()
printGrid iog = iog >>= (printMaze . toMaze)
