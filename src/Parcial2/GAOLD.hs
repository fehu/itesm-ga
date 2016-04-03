-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.GA
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Parcial2.GA (

) where

import GeneticAlgorithm
import Parcial2.Problem

import System.Random

-----------------------------------------------------------------------------

type Node = LabyrinthNode (Int, Int)

data GA = GA Labyrinth2D

instance GeneticAlgorithm GA where type Gene GA = Node
                                   type Chromosome GA = [Gene GA]
                                   listGenes = id

                                   initialPopulation = do rLen <- randomIO :: IO Int

                                                          undefined


