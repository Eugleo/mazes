{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Draw
  ( Config(..)
  , defaults
  , genPng
  )
where

import Grid (Wall(..), Orientation(..))
import Maze.DepthFirst (maze)
import Control.Monad (forM_)

import Graphics.Rendering.Cairo (Render)
import qualified Graphics.Rendering.Cairo as Cairo

-- | Settings that control what the resulting picture looks
--   like. Sizes are in pixels.
data Config =
  Config { step   :: Int -- ^ the size of each cell
  , wall   :: Int -- ^ how wide the wall lines are
  , width  :: Int -- ^ how wide the canvas is
  , height :: Int -- ^ how high the canvas is
  }

-- | Settings that look okay, producing reasonably sized images for
--   40 × 40 mazes.
defaults :: Config
defaults =
  Config { step =   10
  , wall = 2
  , width = 404
  , height = 404
  }

-- | This draws a rectangle, coercing ints into doubles.
rectangle :: (Integral n) => n -> n -> n -> n -> Render ()
rectangle x y w h =
  Cairo.rectangle
  (fromIntegral x)
  (fromIntegral y)
  (fromIntegral w)
  (fromIntegral h)

-- | Renders a maze, which is specified as a list of walls. The order
--   in the list shouldn't matter.
renderMaze :: Config -> [Wall] -> Render ()
renderMaze Config {..} walls = do
  Cairo.setSourceRGB 0.02 0.24 0.54
  Cairo.setLineWidth 5

  rectangle 0 0 width height
  Cairo.stroke

  forM_ walls $ \ (Wall (x, y) dir) -> case dir of
    Horizontal -> rectangle (x * step) (y * step) (step + wall) wall >> Cairo.fill
    Vertical   -> rectangle (x * step) (y * step) wall (step + wall) >> Cairo.fill

-- | Export a maze, given as a list of walls, to PNG.
mazePng :: Config -> FilePath -> [Wall] -> IO ()
mazePng config@Config {..} file walls =
  Cairo.withImageSurface Cairo.FormatARGB32 width height $ \ surface -> do
  Cairo.renderWith surface $ renderMaze config walls
  Cairo.surfaceWriteToPNG surface file

-- | Generate a maze image with a maze of the given dimensions.
genPng :: Config -> FilePath -> Int -> Int -> IO ()
genPng config file width height = do
  m <- maze width height
  mazePng config file m
