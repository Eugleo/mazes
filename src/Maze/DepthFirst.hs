{-# LANGUAGE ViewPatterns #-}

module Maze.DepthFirst
  ( maze
  , edfsR
  , Graph.labEdges
  , (\\)
  )
where

import Grid (grid, Wall(..), normalize, lNeighbors', ghead)
import Control.Applicative ((<$>))
import Control.Monad.Random (MonadRandom)
import Data.Graph.Inductive (Gr, match)
import qualified Data.Graph.Inductive as Graph
import Data.List ((\\))
import System.Random.Shuffle

-- | Return a random path through edges of a graph
edfsR :: MonadRandom m => Graph.Node -> Gr n e -> m [Graph.LEdge e]
edfsR start (match start -> (Just ctx, graph)) =
  normalize <$> go (lNeighbors' ctx) graph
 where
  go []             _                        = return []
  go _              g | Graph.isEmpty g      = return []
  go ((p, n, l):ns) (match n -> (Just c, g)) = do
    edges <- shuffleM $ lNeighbors' c
    ((p, n, l):) <$> go (edges ++ ns) g
  go ns g = go (tail ns) g

-- | Using the edfsR algorithm, and a filled maze grid, generate a maze
maze :: MonadRandom m => Int -> Int -> m [Wall]
maze w h = map Graph.edgeLabel <$> generate
 where
  generate = (Graph.labEdges graph\\) <$> edfsR (ghead graph) graph
  graph    = grid w h
