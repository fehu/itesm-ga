
module Main where

import qualified Parcial2.App as Parcial2

import System.Environment

-----------------------------------------------------------------------------

main = Parcial2.appMain "ga-labyrinth" =<< getArgs

