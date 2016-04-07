-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.Tikz.DrawCrossover
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module Parcial2.Tikz.DrawCrossover where

import Parcial2.Labyrinth
import Parcial2.Tikz
import Parcial2.Examples.Data

import Data.Tuple (swap)

-----------------------------------------------------------------------------



tikzCromosome :: Int -> [TikzAttr] -> [Point2D] -> [Bool] -> TikzExpr
tikzCromosome _ _ genes conns | length genes /= length conns + 1 = error $
                                                                   "wrong list sizes in"
                                                                ++ " `tikzCromosome`"
tikzCromosome chain attrs genes conns = tikzScope [ "start chain=1 going right"
                                                  , "node distance=-0.15mm"
                                                  ]
                                $ do let conns' = map Just conns ++ [Nothing]
                                         onChain = "on chain=" ++ show chain
                                     (g, mbConn) <- genes `zip` conns'
                                     let gene = tikzNode (onChain:attrs)
                                                         (show g)
                                                         Nothing
                                                         (show g)
                                     let conn = case mbConn of
                                            Just b -> tikzNode [onChain] ('c':show g) Nothing
                                                               (if b then "+" else "-")
                                            _      -> TikzExpr []
                                     [gene, conn]


tikzRoute :: [TikzAttr] -> Int -> (Point2D, Point2D) -> Bool -> TikzExpr
tikzRoute attrs chain pts reversed = tikzNode' (fit:attrs) ""
    where (left, right) = if reversed then swap pts else pts
          fit = "fit=" ++ unwords [ strRound $ show left ++ ".north west"
                                  , strRound $ show left ++ ".south west"
                                  , strRound $ show right ++ ".north east"
                                  , strRound $ show right ++ ".south east"
                                  ]


tikzRoutes :: [TikzAttr] -> Int -> [([TikzAttr], ((Point2D, Point2D), Bool))] -> TikzExpr
tikzRoutes attrsScope chain rts = tikzScope ("fill opacity=0.6" :attrsScope)
                                . map f $ rts
    where f (attrs, pt) = uncurry (tikzRoute attrs chain) pt





