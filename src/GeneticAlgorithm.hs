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

                                stopCriteria :: ga -> [Fitness ga] -> IO Bool

                                newGA :: InputData ga -> IO ga

                                crossover ga c1 = fst . crossover' ga c1





-- | Underlying list must be ordered descending.
newtype Assessed chrom fit = Assessed [(chrom, fit)]

-- | Creates a new 'Assessed', sorting the given list by 'Fitness' (descending).
assessed :: (Ord fit) => [(chrom, fit)] -> Assessed chrom fit
assessed = Assessed . sortWith snd

unwrapAssessed (Assessed l) = l

popSize (Assessed l) = length l

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

        selectIntact    :: ga -> Assessed chrom fit -> IO [chrom]
        selectCrossover :: ga -> Assessed chrom fit -> IO [(chrom, chrom)]
        selectMutate    :: ga -> Assessed chrom fit -> IO [chrom]

        selectResult    :: ga -> Assessed chrom fit -> (res, DebugData ga)

        initHook        :: ga -> Int -> IO ()
        iterationHook   :: ga -> IO ()


        initHook _ _ = return ()
        iterationHook _ = return ()

        initialPopulation ga pop = sequence $ do _ <- [1..pop]
                                                 return $ randomChromosome ga

        runGA ga popSize = do
            initHook ga popSize
            runGA' ga =<< initialPopulation ga popSize



runGA' ga pop = do
    let fit = assessed $ map (id &&& fitness ga) pop

    iterationHook ga

    stop <- stopCriteria ga . map snd $ unwrapAssessed fit

    intact  <- selectIntact ga fit
    cross   <- selectCrossover ga fit
    mut     <- selectMutate ga fit


    print $ length intact
    print $ length cross
    print $ length mut

    mutated <- mapM (mutate ga) mut

    print $ length mutated

    let pairToList (x,y) = [x,y]
        newPop  =  intact
                ++ concatMap (pairToList . uncurry (crossover ga)) cross
                ++ mutated

    putStrLn $ "stop = " ++ show stop

    if stop  then return $ selectResult ga fit
             else do putStrLn "run rec GA"
                     runGA' ga newPop



