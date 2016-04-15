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
import Data.Either
import Text.Read (readMaybe)

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

                   , childrenOf  :: Maybe ChildSide
                   , childrenIds :: Maybe [Int]

                   , childrenExt :: Maybe ChildSide
                   }

                   deriving Show



data ChildSide = Fst | Snd deriving (Show, Read, Eq)

instance DefaultSingleParser ChildSide where
    singleParser = SingleParser "ChildSide" readMaybe



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
                        , Opt optChildren
                        , Opt optChildrenIds
                        , Opt optExtChildren
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

optChildren :: Optional1 ChildSide
optChildren = optional "x" [] [ "Generate crossover children, based on the parent given" ]
                              [ "crossover parent, values: `Fst`, `Snd`" ]

optChildrenIds :: OptionalVar Int
optChildrenIds = variable "" ["x-children"] ["Generate crossover children for given indices."
                                            , "`-x` must be specified." ]
                          "index" [ "children indicies." ]

optExtChildren :: Optional1 ChildSide
optExtChildren = optional "e" ["xx-children"] [ "Generate crossover extension children, for the parent given" ]
                              [ "crossover parent, values: `Fst`, `Snd`" ]

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

              (_, dd@(ddr, (ddx, ddxx))) = crossover' ga (fst chrom1) (fst chrom2)

              routes = let rts = if onlySources setup
                                  then prepareRoutes ddr [] []
                                  else prepareRoutes' ddr [] []
                           get i l = [l !! i]
                     in case singleRoute setup of
                                Just i -> first (get i) . second (get i) $ rts
                                _      -> rts

              getExtrs sr@(_, rev) = first (head &&& last) sr
                    where f = if rev then last &&& head else head &&& last
              f = map getExtrs

              pic = case (childrenExt setup, childrenOf setup) of
                (Just side, Nothing) -> drawExtChildren (gaLabyri ga) side ddxx
                (Nothing, Just side) -> childrenPic side
                (Just _, Just _)     -> error "cannot use `-x` and `-e` at the same time"
                _                    -> parentsPic

--              childrenOf setup
--              pic = (parentsPic `maybe` childrenPic $ childrenOf setup)

              parentsPic = tikzPicture []
                  $ uncurry ( tikzCrossover (shift setup)
                                            (repeatColors setup)
                                            (routeColorInd setup)
                                            1 chrom1 2 chrom2
                            )
                            (first f. second f $ routes)

              childrenPic side = let draw = drawChildren (childrenIds setup) ddx
                        in case side of Fst -> draw isLeft
                                        Snd -> draw isRight

              writeIt f s = do writeFile f s
                               putStrLn $ "Wrote file " ++ f

          print ddxx

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

              , childrenOf  = opts `get` optChildren
              , childrenIds = varArgs <$> opts `get` optChildrenIds

              , childrenExt = opts `get` optExtChildren
              }
            Left errs -> exitError errs


exitError errs = error $ unlines (("[ERROR ] " ++) <$> errs)
                     ++ "\n\t run '" ++ appName ++ " -h' for help."

-----------------------------------------------------------------------------

prepareRoutes [] accFst accSnd = (reverse accFst, reverse accSnd)
prepareRoutes (srp:t) accFst accSnd = uncurry (prepareRoutes t) next
    where next = case srp of
            SubRoutes _ _ (Left donor) _ -> (donor:accFst, accSnd)
            SubRoutes _ _ (Right donor) _ -> (accFst, donor:accSnd)


prepareRoutes' [] accFst accSnd = (reverse accFst, reverse accSnd)
prepareRoutes' (srp:t) accFst accSnd = uncurry (prepareRoutes' t) next
    where next = case srp of
            SubRoutes _ _ (Left x) (Right y) -> (x:accFst, y:accSnd)
            SubRoutes _ _ (Right x) (Left y) -> (y:accFst, x:accSnd)

-----------------------------------------------------------------------------

conns l = map (`edgeOf` l) . lPairs
labelNode id y = tikzNode [] id (Just $ AbsPos (-2) (-y))

drawChildren mbIds dta thisSide = TikzExpr $ concatMap ((pref:) . picSurround) draw
    where pref = "\n\\\\"
          picSurround = extract . tikzCmd "fbox" []
                      . extract . tikzCmd "resizePicture" []
                      . extract

          ddata' = (\(SubRoutes _ _ _ target, _) -> thisSide target) `filter` dta
          ddata = first (subRoutesBoth &&& subRoutesLabyrinth) <$> reverse ddata'

          draw = do (((src', target'), l), mbRes) <- ddata
                    let rev x = if snd x then reverse $ fst x else fst x
                        src = rev src'
                        target = rev target'

                        tSrc = tikzCromosome 0 [] src (conns l src)
                        tTar = tikzCromosome 1 [] target (conns l target)

                        tChildFail = tikzNode [] "fail" (Just $ AbsPos 5 (-2)) "ningún"
                        tChildSucc = uncurry (tikzCromosome 2 []) . (id &&& conns l)
                        tChild = tChildFail `maybe` tChildSucc $ mbRes


                    return $ tikzPicture [] [ labelNode "src" 0 "Donado: "
                                            , tSrc
                                            , newline
                                            , labelNode "tar" 1 "Remplacado: "
                                            , tTar
                                            , newline
                                            , labelNode "res" 2 "Hijo: "
                                            , tChild
                                            ]

-----------------------------------------------------------------------------

drawExtChildren l Fst = drawExtChildren' l . fst -- (p@(mbFstLeft, mbFstRight), _)
drawExtChildren l Snd = drawExtChildren' l . snd -- (_, p@(mbSndLeft, mbSndRight))

drawExtChildren' l (mbLeft, mbRight) = tikzPicture [] [
    labelNode "left" 0 "Izquierda: "
  , drawExtResult l 0 mbLeft
  , labelNode "right" 1 "Derecha: "
  , drawExtResult l 1 mbRight
  ]

drawExtResult l chain (Just new) = tikzCromosome chain [] new (conns l new)
drawExtResult _ chain _          = tikzNode [] ("fail" ++ show chain) (Just $ AbsPos 5 (-chain)) "no aplicó"

