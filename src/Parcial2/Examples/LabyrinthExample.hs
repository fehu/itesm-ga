module Main where

import Parcial2.Labyrinth
import Parcial2.Tikz
import Parcial2.Tikz.DrawLabyrinth

import Parcial2.Examples.Data

import Control.Arrow
import System.Environment (getArgs)

-----------------------------------------------------------------------------

main = do args <- getArgs
          case args of []  -> putStrLn showExample
                       [f] -> do writeFile f showExample
                                 putStrLn $ "wrote example to " ++ f
                       ["--map", f] -> do writeFile f showExampleMap
                                          putStrLn $ "wrote example map to " ++ f

-----------------------------------------------------------------------------


showExampleMap = tikzLabyrinth (labyrinth2D labyrinthExample) [] []

showExample = tikzLabyrinth (labyrinth2D labyrinthExample)
                            (map (second (map (first mkpnt . second mkpnt))) chromosomeExamples)
                            [ "draw opacity=0.6" ]


