-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.Tikz.DrawLabyrinth
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module Parcial2.Tikz.DrawLabyrinth where

import Parcial2.Labyrinth
import Parcial2.Tikz

import Data.List (intercalate)
import qualified Data.Set as Set


-----------------------------------------------------------------------------

type Color = String
type ChromosomeExample = (Color, [(Point2D, Point2D)])

tikzLabyrinth :: Labyrinth2D -> [ChromosomeExample] -> [TikzAttr] -> String
tikzLabyrinth l exs attrs = intercalate "\n" . extract . tikzPicture [] $ [
    tikzStyles [ "point"   --> ["draw"]
               , "initial" --> ["draw", "circle", "fill=green!50", "inner sep=2pt"]
               , "target"  --> ["draw", "circle", "fill=blue!50", "inner sep=2pt"]
               ]
  , newline
  ]
  ++ map node (Set.toList $ nodes l)
  ++ [ newline
     , tikzScope [] $ map edge (Set.toList $ edges l)
     , newline
     ]
  ++ zipWith (chromosomeExample attrs) exs (reverse [1..length exs ])

  where node pnt@(Point2D (x,y)) =
            let clazz = case pnt of p | p == initial l -> "initial"
                                    p | p == target l  -> "target"
                                    _                  -> "point"
            in tikzNode [ clazz ]
                        (show pnt)
                        (Just $ AbsPos x y)
                        ("\\footnotesize " ++ show pnt)
        edge (x, y) = tikzEdge (show x) (show y) []

chromosomeExample attrs (color, es) w = tikzScope attrs' $ map route es
    where route (x, y) = tikzEdge (show x) (show y) []
          attrs' = [ "color=" ++ color
                   , "line width=" ++ show w ++ "pt"
                   ]
                  ++ attrs

-----------------------------------------------------------------------------


