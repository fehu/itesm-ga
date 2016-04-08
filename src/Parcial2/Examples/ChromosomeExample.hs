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

import Data.List (elemIndex)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

main = do -- print $ uncurry (tikzCromosome 1 ["draw"]) chExViolet
--          print $ tikzRoute ["fill=green!50", "fill opacity=0.6"] 1
--                            (mkpnt (0,2) --> mkpnt (4,6)) False
--          print $ tikzRoute ["fill opacity=0.6", "fill=blue!50"] 1
--                            (mkpnt (6,1) --> mkpnt (3,2)) True

--          let routes = splitRoutes (labyrinth2D labyrinthExample) (fst chExViolet)
--              routes' = map ((head &&& last) &&& const False ) routes
--              attrs = map (\c -> ["fill =" ++ lighter c 50]) stdColors
--
--          print $ tikzRoutes [] 1 $ attrs `zip` routes'

          print $ tikzCrossover 1 chExViolet
                                2 chExOrange
                                undefined
                                undefined


subseq xs l r = take (r - l) . drop l


foo l ch1 ch2 = let rs1 = splitRoutes l ch1
                    rs2 = splitRoutes l ch2

                    set1 = Set.fromList ch1
                    set2 = Set.fromList ch2
                    cs = Set.toList $ Set.intersection set1 set2

                    isrs = do x <- cs
                              y <- cs
                              let valid = any ((&&) <$> (x `elem`) <*> (y `elem`))
                                  valid1 = valid rs1
                                  valid2 = valid rs2

                                  valid' ch = let Just ix = x `elemIndex` ch
                                                  Just iy = y `elemIndex` ch

                                                  (left, right, rev) = if ix < iy then (ix,iy,False)
                                                                                  else (iy,ix,True)
                                         in (subseq ch left right, rev)

                                  valid1' = if valid1 then Just $ valid' ch1 else Nothing
                                  valid2' = if valid2 then Just $ valid' ch2 else Nothing

                              if x /= y && (valid1 || valid2)
                                then return ((x,y), (valid1', valid2'))
                                else []


--                    rs1 = splitRoutes l ch1
--                    rs2 = splitRoutes l ch2

--                    re1 = [r | r <- rs1, r `notElem` rs2]
--                    re2 = [r | r <- rs2, r `notElem` rs1]

--                in (splitRoutes l re1, splitRoutes l re2)
                in undefined
-----------------------------------------------------------------------------

--data ChromExample


--showExample :: [TikzAttr] -> Int -> String
--showExample





