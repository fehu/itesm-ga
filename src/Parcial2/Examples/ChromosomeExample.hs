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

import Data.Tuple (swap)
import Data.List (elemIndex)
import qualified Data.Set as Set

import System.Environment
import CArgs

-----------------------------------------------------------------------------

data Setup = Setup { chromX :: String
                   , chromY :: String

                   , file :: Maybe FilePath

                   , onlySources :: Bool

                   , shift :: Maybe (Int, String)

                   , repeatColors :: Bool
                   , routeColorInd :: Maybe Int

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
                        , Opt optRepColors
                        , Opt optColorInd
                        , Opt optSingleRoute
                        , Opt helpArg
                        ]
  }


optOutFile :: Optional1 Text
optOutFile = optional "o" ["write"]  ["Output file"] ["specify output file"]

optOnlySources = optionalFlag "" ["only-sources"] ["Generate only source sub-routes"]
optRepColors   = optionalFlag "" ["repeat-colors"] ["Repeat colors at each chromosome"]

optShift :: Optional '[Int, Text] (Int, String)
optShift = optional2 "" ["shift"] ["Shift sub-routes overlays vertically"]
                                  "value" [ "a numeric value" ]
                                  "unit" [ "one of: cm, pt, em" ]
                     (flip $ flip (,) . ensureUnit . text2str)

ensureUnit u = if u `elem` ["cm", "pt", "em"] then u
                else error $ "unknown unit '" ++ u ++ "'"

optColorInd :: Optional1 Int
optColorInd = optional "c" ["color-index"] ["Set route color"] ["`stdColors` index"]

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
                  $ uncurry (tikzCrossover (shift setup)
                                           (repeatColors setup)
                                           (routeColorInd setup)
                                           1 chrom1 2 chrom2
                            )
                            (first (map f) . second (map f) $ routes)
                    where f p = case subRoute2Pair p of
                                    sr@(_, rev) -> first (if rev then last &&& head
                                                                 else head &&& last)
                                                   sr

              writeIt f s = do writeFile f s
                               putStrLn $ "Wrote file " ++ f

          print dd

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
              , repeatColors = opts `flagSet` optRepColors

              , shift = opts `get` optShift
              , routeColorInd = opts `get` optColorInd

              , singleRoute = opts `get` optSingleRoute
              }
            Left errs -> exitError errs


exitError errs = error $ unlines (("[ERROR ] " ++) <$> errs)
                     ++ "\n\t run '" ++ appName ++ " -h' for help."

-----------------------------------------------------------------------------

prepareRoutes' _ _ [] accFst accSnd = (accFst, accSnd)
prepareRoutes' chrom1 chrom2 (srp:t) accFst accSnd =
    prepareRoutes' chrom1 chrom2 t accFst' accSnd'
    where -- r mbRoute chrom = case mbRoute of Just route           -> first (head &&& last) route
--                                            Just route@(_, True) -> first (last &&& head) route
--                                            Just route           -> first (head &&& last) route
--                                            _                    -> route' chrom
          route' chrom = let (Just i1, Just  i2) = first (`elemIndex` fst chrom)
                                                 . second (`elemIndex` fst chrom)
                                                 $ subRoutes srp
                        in if i1 < i2 then (subRoutes srp, False)
                                      else (swap $ subRoutes srp, True)
--          rFst = r mbFst chrom1
--          rSnd = r mbSnd chrom2

--          rFst' = r Nothing chrom1
--          rSnd' = r Nothing chrom2

          (fst', snd') = subRoutes srp
          (accFst', accSnd') = case srp of
                SubRouteBiDir {} -> (fst':snd':accFst, snd':fst':accSnd)
                SubRouteUniDir{} -> (fst':accFst, snd':accSnd)

prepareRoutes [] accFst accSnd = (accFst, accSnd)
prepareRoutes (srp:t) accFst accSnd = case srp of
        SubRouteBiDir {} ->
                            uncurry (prepareRoutes t) . first (:accFst) . second (:accSnd)
                            $ subRoutes srp
        SubRouteUniDir{subRouteSource = src} ->
                            uncurry (prepareRoutes t)
                                    $ case src of   Left r  -> (r:accFst, accSnd)
                                                    Right r -> (accFst, r:accSnd)


--    prepareRoutes t (acc' accFst mbFst) (acc' accSnd mbSnd)
--    where acc' a mb = case mb of Just route -> first (head &&& last) route :a
--                                 _          -> a


