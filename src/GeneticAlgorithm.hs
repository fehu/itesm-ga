-----------------------------------------------------------------------------
--
-- Module      :  GeneticAlgorithm
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module GeneticAlgorithm where

class GeneticAlgorithm ga where type InputData ga :: *
                                type ResultData ga :: *

                                type Gene ga :: *
                                type Chromosome ga :: *
                                listGenes :: Chromosome ga -> [Gene ga]

                                type CrossoverChildren ga :: * -> *

                                initialPopulation :: IO [Chromosome ga]

                                fitness :: Chromosome ga -> Double
                                crossover :: Chromosome ga -> Chromosome ga
                                          -> CrossoverChildren ga (Chromosome ga)
                                mutate :: Chromosome ga -> Chromosome ga

                                stopCriteria :: [Chromosome ga] -> Bool

                                newGA :: InputData ga -> ga
                                runGA :: ga -> IO (ResultData ga)

--runGA' :: ga -> [Chromosome ga] -> IO (ResultData ga)
--runGA' ga chs | stopCriteria chs =


