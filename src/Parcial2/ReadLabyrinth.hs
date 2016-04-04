-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.ReadLabyrinth
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- | Asume que el laberinto será un archivo de texto, en donde se definirán
--    una serie de nodos y una matriz de adyacencia. Para ello, se utilizará el siguiente
--    formato:
--
-- a. Línea 1: un número N que indica cuantos nodos habrá en el laberinto.
-- b. Siguientes N líneas: vi1, vi2, ….,viN (cada vij es un 0 o 1, donde 0
--    representa que no existe adyacencia entre el nodo de la i-ésima línea y en
--    nodo de la j-ésima columna).
-- c. Siguiente línea: vI, vF (dos números, donde vI representa el nodo de
--    partida del agente robótico y vF representa el nodo al cual tiene que
--    llegar).
-- d. Siguientes N líneas: x,y (donde xi,yi representan las coordenadas donde
--    se encuentra en nodo i-ésimo).
--

module Parcial2.ReadLabyrinth where

import Data.List (transpose)
import Text.ParserCombinators.Parsec
import Text.Parsec.Error

-----------------------------------------------------------------------------


data LabyrinthDescription = LabyrinthDescription Int
                                                 [(Int, Int)]
                                                 (Int, Int)
                                                 [(Int, Int)]



buildConnections :: [[Bool]] -> [(Int, Int)]
buildConnections bss = let bss' = transpose bss
                        in do (bs, bs', x) <- zip3 bss bss' [0..]
                              (b, b', y) <- zip3 bs bs' [0..]

                              if b /= b' then error $ "inconsistent connection values at "
                                                   ++ show (x,y) ++ " and " ++ show (y,x)

                                else if b then return (x,y)
                                          else []


-----------------------------------------------------------------------------

parseLabyrinth s = let r = parse labyrinthFile "Failed to parse Labyrinth!" s
                 in case r of Left err -> Left $ messageString <$> errorMessages err
                              Right ok -> Right ok

labyrinthFile :: GenParser Char st LabyrinthDescription
labyrinthFile = do n' <- many1 digit
                   let n = read n' :: Int

                   newline
                   conns <- count n $ do r <- many $ do b <- digit
                                                        readComma
                                                        return $ case b of '0' -> False
                                                                           '1' -> True
                                         newline
                                         return r

                   (vI, vF) <- readPair
                   newline

                   coords <- count n $ do r <- readPair
                                          newline
                                          return r

                   return $ LabyrinthDescription n
                                                 (buildConnections conns)
                                                 (vI, vF)
                                                 coords


readComma = do spaces
               oneOf [',']
               spaces

readPair :: (Read a) => GenParser Char st (a, a)
readPair  = do a <- many1 digit
               readComma
               b <- many1 digit
               return (read a, read b)



