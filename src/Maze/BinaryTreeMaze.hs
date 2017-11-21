{-# LANGUAGE ViewPatterns #-}

module BinaryTreeMaze where

import Grid (grid, Wall(..), normalize, lNeighbors', ghead)
import Control.Applicative ((<$>))
import Control.Monad.Random (MonadRandom)
import Data.Graph.Inductive (Gr, match)
import qualified Data.Graph.Inductive as Graph
import Data.List ((\\))
import System.Random.Shuffle

-- | Return Cell with either the nothern or the eastern border Open
cellR :: IO Cell
cellR = fmap (cells!!) (randomRIO (0, 1))
  where cells = [Cell Open Closed, Cell Closed Open]

-- | Return a grid, whose paths represend a BT
binaryTreeGrid :: Int -> Int -> IO Grid
binaryTreeGrid c r = body
 where
  -- first row needs to have all nothern borders closed
  firstRow = replicate (c - 1) (Cell Closed Open) ++ [Cell Closed Closed]
  body     = (firstRow:) <$> replicateM (r - 1) row
  -- every row needs to have the eastern border of the rightmost cell closed
  row      = (++[Cell Open Closed]) <$> replicateM (c - 1) cellR

-- | Convert the Grid to a Maze and print it
printGrid :: IO Grid -> IO ()
printGrid iog = iog >>= (printMaze . toMaze)

btR :: Graph.Node -> Gr n e -> [Graph.LEdge e]
btR start (match start -> Just (ctx, graph)) = undefined
