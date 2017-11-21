{-# LANGUAGE ViewPatterns #-}

module Grid
  ( grid
  , Wall(..)
  , Orientation(..)
  , ghead
  , normalize
  , lNeighbors'
  )
where

import Data.Graph.Inductive (Gr, matchAny)
import qualified Data.Graph.Inductive as Graph

data Orientation = Horizontal | Vertical deriving (Show, Eq)
data Wall = Wall (Int, Int) Orientation deriving (Eq)
type Grid = Gr () Wall

instance Show Wall where
  show (Wall xy Horizontal) = show xy ++ " —"
  show (Wall xy Vertical) = show xy ++ " |"

-- | Return a random node from nonempty graph
ghead :: Gr a b -> Graph.Node
ghead graph | Graph.isEmpty graph            = error "Empty graph!"
ghead (matchAny -> ((_, node, _, _), _)) = node

-- | Return all edges going in or out of a node (context)
lNeighbors' :: Graph.Context n e -> [Graph.LEdge e]
lNeighbors' c = [ (p, n, l) | (n, l) <- Graph.lpre' c ++ Graph.lsuc' c ]
  where p = Graph.node' c

-- | Normalize all edges - make them go in one direction, i.e. (higer, lower)
normalize :: [Graph.LEdge e] -> [Graph.LEdge e]
normalize = map swap
 where
  swap (n, n', l) | n < n'    = (n', n, l)
                  | otherwise = (n, n', l)

-- | A graph representing a maze with all borders closed
-- nodes = individual rooms in the maze
-- edges = the walls between the rooms
grid :: Int -> Int -> Grid
grid w h = Graph.mkGraph nodes edges
 where
  nodeNs = [0 .. w * h - 1]
  nodes  = [ (n, ()) | n <- nodeNs ]
  edges  = horizontal ++ vertical
  horizontal =
    [ (n', n, wall n' Vertical)
    | n  <- nodeNs
    , n' <- nodeNs
    , n' - n == 1 && n' `mod` w /= 0
    ]
  vertical =
    [ (n', n, wall n' Horizontal) | n <- nodeNs, n' <- nodeNs, n' - n == w ]
  wall n = let (x, y) = n `divMod` w in Wall (y, x)
