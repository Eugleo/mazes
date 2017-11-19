{-# LANGUAGE ViewPatterns #-}

module BetterGrid where

import Control.Applicative ((<$>))
import Control.Monad.Random (MonadRandom)
import Data.Graph.Inductive
       (Gr, (&), labEdges, match, matchAny, order)
import qualified Data.Graph.Inductive as Graph
import Data.List ((\\))
import System.Random.Shuffle

ghead :: Gr a b -> Graph.Node
ghead graph | Graph.isEmpty graph            = error "Empty graph!"
ghead (matchAny -> ((_, node, _, _), graph)) = node

dfs :: Graph.Node -> Gr a b -> [Graph.Node]
dfs s = go [s]
 where
  go []     _                        = []
  go _      g | Graph.isEmpty g      = []
  go (n:ns) (match n -> (Just c, g)) = n : go (Graph.neighbors' c ++ ns) g
  go (_:ns) g                        = go ns g

edfs :: Graph.Node -> Gr n e -> [Graph.LEdge e]
edfs start (match start -> (Just ctx, graph)) = normalize
  $ go (lNeighbors' ctx) graph
 where
  go [] _                   = []
  go _  g | Graph.isEmpty g = []
  go ((p, n, l):ns) (match n -> (Just c, g)) =
    (p, n, l) : go (lNeighbors' c ++ ns) g
  go (_:ns) g = go ns g

lNeighbors' :: Graph.Context n e -> [Graph.LEdge e]
lNeighbors' c = [ (p, n, l) | (n, l) <- Graph.lpre' c ++ Graph.lsuc' c ]
  where p = Graph.node' c

normalize :: [Graph.LEdge e] -> [Graph.LEdge e]
normalize = map swap
 where
  swap (n, n', l) | n < n'    = (n', n, l)
                  | otherwise = (n, n', l)

edfsR :: MonadRandom m => Graph.Node -> Gr n e -> m [Graph.LEdge e]
edfsR start (match start -> (Just ctx, graph)) =
  normalize <$> go (lNeighbors' ctx) graph
 where
  go []             _                        = return []
  go _              g | Graph.isEmpty g      = return []
  go ((p, n, l):ns) (match n -> (Just c, g)) = do
    shuffledNeigbors <- shuffleM (lNeighbors' c ++ ns)
    ((p, n, l):) <$> go shuffledNeigbors g
  go (_:ns) g = go ns g

data Orientation = Horizontal | Vertical deriving (Show, Eq)

data Wall = Wall (Int, Int) Orientation deriving (Show, Eq)

type Grid = Gr () Wall

-- | A graph representable by rectangle with all borders closed
grid :: Int -> Int -> Grid
grid w h = Graph.mkGraph nodes edges
 where
  nodeNs = [0 .. w * h - 1]
  nodes  = [ (n, ()) | n <- nodeNs ]
  edges  = horizontal ++ vertical
  horizontal =
    [ (n, n', wall n' Horizontal)
    | n  <- nodeNs
    , n' <- nodeNs
    , n' - n == 1 && n' `mod` w /= 0
    ]
  vertical =
    [ (n, n', wall n' Vertical) | n <- nodeNs, n' <- nodeNs, n' - n == w ]
  wall n = let (x, y) = w `divMod` n in Wall (y, x)

generate :: MonadRandom m => Int -> Int -> m [Graph.LEdge Wall]
generate w h = (Graph.labEdges graph\\) <$> edfsR (ghead graph) graph
  where graph = grid w h

maze :: MonadRandom m => Int -> Int -> m [Wall]
maze w h = map Graph.edgeLabel <$> generate w h




