-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.App
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module Parcial2.App where

import Data.Maybe
import Data.Typeable

import GeneticAlgorithm
import Parcial2.Labyrinth

import CArgs


-----------------------------------------------------------------------------

appDescr = [ "Searches the shortest path in a labyrinth with Genetic Algorithm." ]

-----------------------------------------------------------------------------
---- Arguments

appArgs = CArgs {
    positionalArguments = Positional "Labyrinth File" text ["path to labyrinth file"]
                       :. Positional "Population Size" int []
                       :. Nil
  , optionalArguments = [
      Opt optChromGenMaxChains
    , Opt optChromGenMaxChainLen
    , Opt optMutateMaxChainsGen
    , Opt optMutateMaxChainLen
    , Opt optMaxUnchangedIter
    , Opt optMaxIters
    , Opt optSelIntactFrac
    , Opt optSelCrossoverFrac
    , Opt optSelMutateFrac
    , Opt optSelFracs
    , Opt helpArg
    ]
  }


-----------------------------------------------------------------------------
---- Options

optChromGenMaxChains :: Optional1 Int
optChromGenMaxChains = optional "" ["gen-max-chains"]
                                   ["Chromosome Generation: maximum chain number."]
                                   []

optChromGenMaxChainLen :: Optional1 Int
optChromGenMaxChainLen = optional "" ["gen-max-chain-len"]
                                     ["Chromosome Generation: maximum chain length."]
                                     []

optMutateMaxChainsGen :: Optional1 Int
optMutateMaxChainsGen = optional "" ["mut-max-chains"]
                                    ["Chromosome Mutation: maximum chains to insert."]
                                    []

optMutateMaxChainLen :: Optional1 Int
optMutateMaxChainLen = optional "" ["mut-max-chain-len"]
                                   ["Chromosome Mutation: maximum insert chain length."]
                                   []

optMaxUnchangedIter :: Optional1 Int
optMaxUnchangedIter = optional "U" ["max-unchanged"]
                                   ["Maximum number of iterations without best fitness change before stopping."]
                                   []

optMaxIters :: Optional1 Int
optMaxIters = optional "I" ["max-iter"]
                           ["Maximum number of iterations."]
                           []


optSelIntactFrac :: Optional1 Float
optSelIntactFrac = optional "" ["frac-intact"]
                               ["Fraction of population left intact."]
                               []

optSelCrossoverFrac :: Optional1 Float
optSelCrossoverFrac  = optional "" ["frac-crossover"]
                                   ["Fraction of population used for crossover."]
                                   []

optSelMutateFrac :: Optional1 Float
optSelMutateFrac = optional "" ["frac-mutation"]
                               ["Fraction of population used for mutation."]
                               []


optSelFracs :: OptionalT3 Float Float Float
optSelFracs = optional3' "f" ["fracs"] ["Set all the fractions at once."]
                             "intact"    ["intact fraction"]
                             "crossover" ["crossover fraction"]
                             "mutation"  ["mutation fraction"]

-----------------------------------------------------------------------------
---- Defaults


readParams opts =
    let orElse :: (Typeable a) => Optional vs a -> a -> a
        opt `orElse` def = fromMaybe def $ opts `get` opt

        intact'     = optSelIntactFrac    `orElse` 0.4
        crossover'  = optSelCrossoverFrac `orElse` 0.3
        mutation'   = optSelMutateFrac    `orElse` 0.3

        (intact, crossover, mutation) = optSelFracs `orElse` (intact', crossover', mutation')

    in GAParams {   gaChromGenMaxChainLen  = optChromGenMaxChainLen `orElse` 5
                ,   gaChromGenMaxChains    = optChromGenMaxChains   `orElse` 3

                ,   gaMutateMaxChainsGen   = optMutateMaxChainsGen  `orElse` 2
                ,   gaMutateMaxChainLen    = optMutateMaxChainLen   `orElse` 3

                ,   gaMaxUnchangedIter     = optMaxUnchangedIter    `orElse` 5
                ,   gaMaxIters             = optMaxIters            `orElse` (10*1000)

                ,   gaSelIntactFrac        = toRational intact
                ,   gaSelCrossoverFrac     = toRational crossover
                ,   gaSelMutateFrac        = toRational mutation

  }


-----------------------------------------------------------------------------
---- Assert


assertPos name x | x < 1 = error $ "'" ++ name ++ "' cannot be less than 1."
assertPos _ _ = return ()

assertUnit name x | x > 1 || x < 0 = error $ "'" ++ name ++ "' must be in [0,1]."
                  | otherwise      = return ()

assertSumOne precision msg vs | abs (1 - sum vs) > precision = error msg
                              | otherwise                    = return ()

assertParams p = do assertPos "gen-max-chain-len" $ gaChromGenMaxChainLen p
                    assertPos "gen-max-chais"     $ gaChromGenMaxChains p
                    assertPos "mut-max-chain-len" $ gaMutateMaxChainLen p
                    assertPos "mut-max-chains"    $ gaMutateMaxChainsGen p
                    assertPos "max-unchanged"     $ gaMaxUnchangedIter p
                    assertPos "max-iter"          $ gaMaxIters p

                    assertUnit "frac-intact"      $ gaSelIntactFrac p
                    assertUnit "frac-crossover"   $ gaSelCrossoverFrac p
                    assertUnit "frac-mutation"    $ gaSelMutateFrac p

                    assertSumOne 0.0001 "The sum of fractions must be 1." $
                        ($p) <$> [ gaSelIntactFrac, gaSelCrossoverFrac, gaSelMutateFrac]



-----------------------------------------------------------------------------
---- Main

appMain appName args = do
    let cargs = parseArgs appArgs args
        params = readParams (optionalValues cargs)

        (file, pop) = case positionalValues cargs of
                Right (file' :. pop' :. Nil) -> (text2str $ posValue file', posValue pop')

    labyrinth' <- readLabyrinth2D file

    let l = case labyrinth' of Left err -> error $ unlines err
                               Right l  -> l

        execGA = printAppResult =<< runApp l params pop

    withHelp appName appDescr appArgs cargs execGA

runApp labyrinth params pop = do
        assertPos "population size" pop
        assertParams params

        ga <- newGA (labyrinth, params) :: IO GA

        runGA ga pop

printAppResult (res, debug) = do
        putStrLn "DEBUG\n=====\n"
        print debug

        putStrLn "RESULT\n======\n"
        print res

