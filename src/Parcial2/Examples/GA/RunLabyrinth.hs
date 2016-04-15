
module Main where

import CArgs

import Parcial2.App
import Parcial2.Examples.Data

import System.Environment

-----------------------------------------------------------------------------

appName = "ga-labyrinth-example-1"
appDescr' = [ "Searches the shortest path in the labyrinth example with Genetic Algorithm." ]

args0 = CArgs{
    positionalArguments = Positional "Population Size" int [] :. Nil
  , optionalArguments = optionalArguments appArgs
  }


main = do cargs <- parseArgs args0 <$> getArgs
          let params = readParams (optionalValues cargs)
              pop = case positionalValues cargs of
                Right (pop' :. Nil) -> posValue pop'


          withHelp appName appDescr' args0 cargs
            $ printAppResult =<< runApp labyrinth params pop

    where labyrinth = labyrinth2D labyrinthExample


