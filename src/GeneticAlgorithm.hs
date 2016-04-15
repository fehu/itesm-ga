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

module GeneticAlgorithm where

import Control.Arrow
import GHC.Exts (sortWith)

data OrdDir = Max | Min

class GeneticAlgorithm ga where type InputData ga :: *
                                type ResultData ga :: *

                                type Gene ga :: *
                                type Chromosome ga :: *

                                type Fitness ga :: *
                                type Target ga :: OrdDir

                                type CrossoverDebug ga :: *

                                listGenes :: Chromosome ga -> [Gene ga]

                                randomChromosome :: ga -> IO (Chromosome ga)

                                fitness   :: ga -> Chromosome ga -> Fitness ga

                                crossover' :: ga -> Chromosome ga -> Chromosome ga
                                           -> ((Chromosome ga, Chromosome ga), CrossoverDebug ga)

                                crossover :: ga -> Chromosome ga -> Chromosome ga
                                                -> (Chromosome ga, Chromosome ga)
                                mutate    :: ga -> Chromosome ga -> IO (Chromosome ga)

                                stopCriteria :: ga -> [Fitness ga] -> Bool

                                newGA :: InputData ga -> ga

                                crossover ga c1 = fst . crossover' ga c1





-- | Underlying list must be ordered descending.
newtype Assessed chrom fit = Assessed [(chrom, fit)]

-- | Creates a new 'Assessed', sorting the given list by 'Fitness' (descending).
assessed :: (Ord fit) => [(chrom, fit)] -> Assessed chrom fit
assessed = Assessed . sortWith snd

unwrapAssessed (Assessed l) = l

class ( GeneticAlgorithm ga
      , res ~ ResultData ga
      , chrom ~ Chromosome ga
      , fit ~ Fitness ga
      , Ord fit
      , target ~ Target ga
      )
  =>
    RunGA ga res chrom fit (target :: OrdDir) where
        type DebugData ga :: *

        runGA :: ga -> Int -> IO (res, DebugData ga)

        initialPopulation :: ga -> Int -> IO [chrom]

        selectIntact    :: ga -> Assessed chrom fit -> [chrom]
        selectCrossover :: ga -> Assessed chrom fit -> [(chrom, chrom)]
        selectMutate    :: ga -> Assessed chrom fit -> [chrom]

        selectResult    :: ga -> Assessed chrom fit -> (res, DebugData ga)


        runGA ga popSize = runGA' ga =<< initialPopulation ga popSize



-- TODO: mutate
runGA' ga pop = let fit = assessed $ map (id &&& fitness ga) pop
                    intact = selectIntact ga fit
                    cross  = selectCrossover ga fit
                    mut    = selectMutate ga fit

                 in if stopCriteria ga . map snd $ unwrapAssessed fit
                      then return $ selectResult ga fit
                      else runGA' ga $ -- new population
                                  intact ++
                                  concatMap ((\(x,y) -> [x,y]) . uncurry (crossover ga)) cross


