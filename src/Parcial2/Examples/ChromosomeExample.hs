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

{-# LANGUAGE FlexibleInstances #-}

module Main where

import GeneticAlgorithm
import Parcial2.Labyrinth
import Parcial2.Tikz
import Parcial2.Tikz.DrawCrossover
import Parcial2.Examples.Data

import Control.Arrow

import Data.List (elemIndex)
import qualified Data.Set as Set

import System.Environment
import CArgs

-----------------------------------------------------------------------------

data Setup = Setup { chromX :: String
                   , chromY :: String

                   , file :: Maybe FilePath

                   , onlySources :: Bool
                   , shift :: Bool
                   , singleRoute :: Maybe Int
                   }

                   deriving Show

-----------------------------------------------------------------------------

argsDescr = CArgs{
    positionalArguments = Positional "chromosome X" text ["X chromosome name"]
                       :. Positional "chromosome Y" text ["Y chromosome name"]
                       :. Nil
  , optionalArguments = [ Opt optOutFile
                        , Opt optOnlySources
                        , Opt optShift
                        , Opt optSingleRoute
                        , Opt helpArg
                        ]
  }


optOutFile :: Optional1 Text
optOutFile = optional "o" ["write"]  ["Output file"] ["specify output file"]

optOnlySources = optionalFlag "" ["only-sources"] ["Generate only source sub-routes"]
optShift       = optionalFlag "" ["shift"] ["Shift sub-routes overlays vertically"]

optSingleRoute :: Optional1 Int
optSingleRoute = optional "r" ["route"] ["Generate only the sub-route specified"]
                                        ["sub-route index"]


-----------------------------------------------------------------------------

appName = "ChromosomeExample"
appDescr = [ "Generates chromosomes crossover examples for Tikz LaTeX package." ]

-----------------------------------------------------------------------------

main = do args <- getArgs
          let cargs = parseArgs argsDescr args
              setup = parseArgs' cargs


              ga = GA (labyrinth2D labyrinthExample) undefined undefined
              chrom1 = prepareChromosomeExample $ chromX setup
              chrom2 = prepareChromosomeExample $ chromY setup
              (_, dd) = crossover' ga (fst chrom1) (fst chrom2)

              routes = let rts = if onlySources setup
                                  then prepareRoutes dd [] []
                                  else prepareRoutes' chrom1 chrom2 dd [] []
                           get i l = [l !! i]
                     in case singleRoute setup of
                                Just i -> first (get i) . second (get i) $ rts
                                _      -> rts
              pic = tikzPicture []
                  $ uncurry (tikzCrossover (shift setup) 1 chrom1 2 chrom2) routes

              writeIt f s = do writeFile f s
                               putStrLn $ "Wrote file " ++ f

          -- print setup

          withHelp appName appDescr argsDescr cargs
            $ maybe (print pic) (`writeIt` show pic) $ file setup


parseArgs' :: CArgValues '[Text, Text] -> Setup
parseArgs' (CArgValues chroms opts optErr) =
    case chroms of
            Right (chrom1 :. chrom2 :. Nil) -> Setup{
                chromX = text2str $ posValue chrom1
              , chromY = text2str $ posValue chrom2

              , file = text2str <$> opts `get` optOutFile

              , onlySources = opts `flagSet` optOnlySources
              , shift       = opts `flagSet` optShift
              , singleRoute = opts `get` optSingleRoute
              }
            Left errs -> exitError errs


exitError errs = error $ unlines (("[ERROR ] " ++) <$> errs)
                     ++ "\n\t run '" ++ appName ++ " -h' for help."

-----------------------------------------------------------------------------

prepareRoutes' _ _ [] accFst accSnd = (accFst, accSnd)
prepareRoutes' chrom1 chrom2 ((chain, (mbFst, mbSnd)):t) accFst accSnd =
    prepareRoutes' chrom1 chrom2 t (rFst:accFst) (rSnd:accSnd)
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




