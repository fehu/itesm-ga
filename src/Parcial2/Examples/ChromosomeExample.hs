-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.Examples.ChromosomeExample
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module Main where

import Parcial2.Labyrinth
import Parcial2.Tikz
import Parcial2.Tikz.DrawCrossover
import Parcial2.Examples.Data

import Control.Arrow

-----------------------------------------------------------------------------

main = do print $ uncurry (tikzCromosome 1 ["draw"]) chExViolet
--          print $ tikzRoute ["fill=green!50", "fill opacity=0.6"] 1
--                            (mkpnt (0,2) --> mkpnt (4,6)) False
--          print $ tikzRoute ["fill opacity=0.6", "fill=blue!50"] 1
--                            (mkpnt (6,1) --> mkpnt (3,2)) True

          let routes = splitRoutes (labyrinth2D labyrinthExample) (fst chExViolet)
              routes' = map ((head &&& last) &&& const False ) routes
              attrs = map (\c -> ["fill =" ++ lighter c 50]) stdColors

          print $ tikzRoutes [] 1 $ attrs `zip` routes'


-----------------------------------------------------------------------------

--data ChromExample


--showExample :: [TikzAttr] -> Int -> String
--showExample





