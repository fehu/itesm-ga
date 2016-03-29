-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.Problem
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module Parcial2.Problem where


import Data.Graph (Graph, Vertex)
import Data.Map (Map)

-----------------------------------------------------------------------------

data LabyrinthNode coord = LabyrinthNode{ nodeId :: Vertex,
                                          nodePosition :: coord
                                        }

type Distance = Double

data Labyrinth coord = Labyrinth { labyrinthGraph :: Graph
                                 , labyrinthNodes :: Map Vertex (LabyrinthNode coord)
                                 , labyrinthDist  :: Vertex -> Vertex -> Distance
                                 }

type Labyrinth2D = Labyrinth (Int, Int)

