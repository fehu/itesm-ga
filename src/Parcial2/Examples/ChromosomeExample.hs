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

import GeneticAlgorithm
import Parcial2.Labyrinth
import Parcial2.Tikz
import Parcial2.Tikz.DrawCrossover
import Parcial2.Examples.Data

import Control.Arrow

import Data.List (elemIndex)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

main = do let ga = GA (labyrinth2D labyrinthExample) undefined undefined
              chrom1 = fst chExViolet
              chrom2 = fst chExOrange
              (_, dd) = crossover' ga chrom1 chrom2

          print dd
          putStrLn ""


          let prepareRoutes' [] accFst accSnd = (accFst, accSnd)
              prepareRoutes' ((chain, (mbFst, mbSnd)):t) accFst accSnd =
                    prepareRoutes t (rFst:accFst) (rSnd:accSnd)
                    where r mbRoute chrom = case mbRoute of Just route -> first (head &&& last) route
                                                            _          -> route' chrom
                          route' chrom = let (i1, i2) = chain
                                        in if i1 < i2 then ((i1,i2), False)
                                                      else ((i2,i1), True)
                          rFst = r mbFst chrom1
                          rSnd = r mbSnd chrom2

              prepareRoutes [] accFst accSnd = (accFst, accSnd)
              prepareRoutes ((chain, (mbFst, mbSnd)):t) accFst accSnd =
                prepareRoutes t (acc' accFst mbFst) (acc' accSnd mbSnd)
                    where acc' a mb = case mb of Just (_, rev) -> (chain,rev):a
                                                 _             -> a

          print $ uncurry (tikzCrossover False 1 chExViolet 2 chExOrange)
                          (prepareRoutes dd [] [])











