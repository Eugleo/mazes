module Grid
  ( Cell(..)
  , Border(..)
  , Grid
  , toMaze
  , printMaze
  ) where

data Border = Open | Closed deriving (Show)

-- | Cell Northern Eastearn
data Cell = Cell Border Border deriving (Show)

-- | Represents the whole maze
type Grid = [[Cell]]

-- | Intermediary type for printing out grids
-- every odd list contains horizontal borders
-- every even list contains vertical borders
type Maze = [[Border]]

-- | Convert the grid of cells to maze of borders
-- each cell is checked twice - first for the horizontal (Nothern) and then
--  for vertical (Eastern) border
toMaze :: Grid -> Maze
toMaze g = body ++ [hBorder]
  where
    body = combine ((map (map hFunc) g)) (map (map vFunc) g)
    hBorder = replicate (length $ head g) Closed
    vFunc (Cell _ x) = x
    hFunc (Cell x _) = x

-- | Return a grid with all borders closed
closedGrid :: Int -> Int -> Grid
closedGrid c r = replicate r . replicate c $ Cell Closed Closed

combine :: [a] -> [a] -> [a]
combine [] ys = ys
combine xs [] = xs
combine (x:xs) (y:ys) = x : y : combine xs ys

-- | Render every border of the maze
prettify :: Maze -> [String]
prettify = helper 1
  where
    helper :: Int -> Maze -> [String]
    helper _ [] = []
    helper n (b:bs)
      | odd n = ('+' : concatMap h b) : helper (n + 1) bs
      -- odd => we're dealing with horizontal borders
      | otherwise = ('|' : concatMap v b) : helper (n + 1) bs
      -- even => we're dealing with vertical borders
    h Closed = "---+"
    h Open = "   +"
    v Closed = "   |"
    v Open = "    "

printMaze :: Maze -> IO ()
printMaze m = putStr $ unlines $ prettify m
