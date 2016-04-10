-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.Examples.Data
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module Parcial2.Examples.Data where

import Parcial2.Labyrinth
import Parcial2.Tikz ((-->))

import qualified Data.Set as Set
import Data.List (nub)

import Control.Arrow

-----------------------------------------------------------------------------

mkpnt = Point2D . first fromInteger . second (negate . fromInteger)

-----------------------------------------------------------------------------

nodesC = [
    (3,2), (4,2), (6,2), (7,2)
  , (3,3), (4,3), (6,3), (7,3)
  , (3,4), (4,4), (6,4), (7,4)
  , (3,5), (4,5), (6,5), (7,5)
  ]

nodesW = [ (1,0), (0,2), (1,2), (0,4), (2,3) ]

nodesN = [ (3,0), (4,0), (4,1), (6,1), (7,1) ]

nodesS = [ (1,6), (3,6), (4,6), (6,6) ]

nodesE = [ (8,0), (8,3), (8,6), (9,3) ]


edgesC = [
  -- horizontal grid
    (3,2) --> (4,2), (4,2) --> (6,2), (6,2) --> (7,2)
  , (3,3) --> (4,3), (4,3) --> (6,3), (6,3) --> (7,3)
  , (3,4) --> (4,4), (4,4) --> (6,4), (6,4) --> (7,4)
  , (3,5) --> (4,5), (4,5) --> (6,5), (6,5) --> (7,5)
  -- vertical grid
  , (3,2) --> (3,3), (3,3) --> (3,4), (3,4) --> (3,5)
  , (4,2) --> (4,3), (4,3) --> (4,4), (4,4) --> (4,5)
  , (6,2) --> (6,3), (6,3) --> (6,4), (6,4) --> (6,5)
  , (7,2) --> (7,3), (7,3) --> (7,4), (7,4) --> (7,5)
  -- inner short route
  , (4,3) --> (6,5)
  -- with W
  , (3,3) --> (2,3)
  -- with N
  , (3,2) --> (3,0), (4,2) --> (6,1), (6,2) --> (6,1), (7,2) --> (7,1)
  -- with S
  , (3,5) --> (3,6), (4,5) --> (6,6), (6,5) --> (6,6)
  -- with E
  , (6,5) --> (8,6)
  ]

edgesW = [
    (0,2) --> (1,2), (0,2) --> (0,4)
  , (0,4) --> (4,6)
  , (1,2) --> (1,0), (1,2) --> (1,6)
  , (1,0) --> (3,0), (1,0) --> (2,3)
  , (2,3) --> (1,6)
  ]

edgesN = [
    (3,0) --> (4,0)
  , (4,0) --> (8,0), (4,0) --> (4,1)
  , (4,1) --> (6,1), (6,1) --> (7,1)
  , (7,1) --> (8,0)
  ]

edgesS = [ (1,6) --> (3,6), (4,6) --> (6,6) ]

edgesE = [ (8,0) --> (8,3), (8,3) --> (9,3), (8,3) --> (8,6) ]


labyrinthExample = Labyrinth nodes' edges' (0,2) (9,3)
    where nodes' = Set.fromList $ nodesC ++ nodesW ++ nodesN ++ nodesS ++ nodesE
          edges' = Set.fromList $ edgesC ++ edgesW ++ edgesN ++ edgesS ++ edgesE


labyrinth2D = mapPoints mkpnt

-----------------------------------------------------------------------------

infixl 2 =->
(a,b) =-> c = [(a,b), (b,c)]

chromosomeExamples = [
    "violet"    --> ( ((0,2) --> (0,4) =-> (4,6))
                   ++ ((3,6) --> (3,5) =-> (4,5))
                   ++ ((3,4) --> (3,3) =-> (4,3))
                   ++ ((3,2) --> (4,2) =-> (6,1))
                   ++ ((7,5) --> (6,5) =-> (8,6))
                    )
  , "orange"    --> [ (0,2) --> (1,2), (2,3) --> (3,3)
                    , (4,0) --> (4,1), (4,2) --> (4,3)
                    , (4,5) --> (6,6), (6,5) --> (6,6)
                    , (7,1) --> (8,0)
                    ]
  , "red"       --> ( ((4,3) --> (4,2) =-> (6,2))
                   ++ ((6,2) --> (6,1) =-> (7,1))
                   ++ ((8,6) --> (8,3) =-> (9,3))
                    )
  , "blue"      --> [ (3,2) --> (3,3), (4,3) --> (6,5)
                    , (6,1) --> (7,1), (7,2) --> (7,3)
                    ]
  , "green"     --> [ (1,0) --> (2,3), (8,0) --> (8,3)
                    , (3,5) --> (4,5), (7,2) --> (7,3)
                    , (1,2) --> (1,0), (4,6) --> (6,6)
                    ]
  ]


-----------------------------------------------------------------------------


prepareChromosomeExample :: String -> ([Point2D], [Bool])
prepareChromosomeExample color = (map mkpnt genes, conns)
    where genes = case color `lookup` chromosomeExamples of
                        Just ps -> nub $ concatMap (\(x,y) -> [x,y]) ps
                        _ -> error $ "No such example chromosome: " ++ color
          conns = (`edgeOf` labyrinthExample) <$> lPairs genes

-----------------------------------------------------------------------------




chExViolet = prepareChromosomeExample "violet"
chExOrange = prepareChromosomeExample "orange"



-----------------------------------------------------------------------------

