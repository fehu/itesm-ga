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

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module GeneticAlgorithm where

--import Data.List
import Data.Ord (Down)

import Control.Arrow
import GHC.Exts

class GeneticAlgorithm ga where type InputData ga :: *
                                type ResultData ga :: *

                                type Gene ga :: *
                                type Chromosome ga :: *

                                type Fitness ga :: *

                                listGenes :: Chromosome ga -> [Gene ga]

                                -- | Denotes number of crossover children.
                                type CrossoverChildren ga :: * -> *

                                initialPopulation :: ga -> IO [Chromosome ga]

                                fitness   :: Chromosome ga -> Fitness ga
                                crossover :: Chromosome ga -> Chromosome ga
                                          -> CrossoverChildren ga (Chromosome ga)
                                mutate    :: Chromosome ga -> Chromosome ga

                                stopCriteria :: [Fitness ga] -> Bool

                                newGA :: InputData ga -> ga

-- | Underlying list must be ordered descending.
newtype Assessed ga = Assessed [(Chromosome ga, Fitness ga)]

-- | Creates a new 'Assessed', sorting the given list by 'Fitness' (descending).
assessed :: (Ord (Fitness ga)) => [(Chromosome ga, Fitness ga)] -> Assessed ga
assessed = Assessed . sortWith (Down . snd)

unwrapAssessed (Assessed l) = l

class (GeneticAlgorithm ga) => RunGA ga where
    type DebugData ga :: *

    runGA :: ga -> IO (ResultData ga, DebugData ga)

    selectIntact    :: Assessed ga -> Assessed ga
    selectCrossover :: Assessed ga -> Assessed ga
    selectMutate    :: Assessed ga -> Assessed ga

    selectResult    :: Assessed ga -> (ResultData ga, DebugData ga)

--    runGA ga = do pop <- initialPopulation ga
--                  let fit = assessed $ map (id &&& fitness) pop
--                      intact = selectIntact fit
--                      cross  = selectCrossover fit
--                      mut    = selectMutate fit
--
--                  if stopCriteria . map snd $ unwrapAssessed fit
--                    then return $ selectResult fit
--                    else  undefined



