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


-----------------------------------------------------------------------------

type Node = LabyrinthNode (Int, Int)

data GA = GA

instance GeneticAlgorithm GA where type Gene GA = Node
                                   type Chromosome GA = [Gene GA]
                                   listGenes = id

                                   initialPopulation = undefined


