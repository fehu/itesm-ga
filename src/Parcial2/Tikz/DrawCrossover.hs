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
tikzCromosome chain attrs genes conns = tikzScope [ "start chain=" ++ show chain
                                                                   ++ " going right"
                                                  , "node distance=-0.15mm"
                                                  , "yshift=" ++ show (-chain*2) ++ "cm"
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


type TikzChrom = ([TikzAttr], [Point2D], [Bool])
type TikzRoute = ([TikzAttr], ((Point2D, Point2D), Bool))


type TikzChrom' = ([Point2D], [Bool])
type TikzRoute' = (((Point2D, Point2D), Bool))



tikzRoutes :: [TikzAttr] -> Int -> [TikzRoute] -> TikzExpr
tikzRoutes attrsScope chain rts = tikzScope ("fill opacity=0.3" :attrsScope)
                                . map f $ rts
    where f (attrs, pt) = uncurry (tikzRoute attrs chain) pt



uncurry3 :: (x -> y -> z -> r) -> (x,y,z) -> r
uncurry3 f (x,y,z) = f x y z

tikzCrossover' :: Int -> TikzChrom -> Int -> TikzChrom
               -> [TikzAttr] -> [TikzRoute]
               -> [TikzAttr] -> [TikzRoute]
               -> TikzExpr
tikzCrossover' chain1 chrom1 chain2 chrom2 rsAttrs1 rs1 rsAttrs2 rs2 =
    TikzExpr $ extract (uncurry3 (tikzCromosome chain1) chrom1)
            ++ ""
             : extract (tikzRoutes rsAttrs1 chain1 rs1)
            ++ ""
             : extract (uncurry3 (tikzCromosome chain2) chrom2)
            ++ ""
             : extract (tikzRoutes rsAttrs2 chain2 rs2)


tikzCrossover :: Bool -> Int -> TikzChrom' -> Int -> TikzChrom'
               -> [TikzRoute'] -> [TikzRoute']
               -> TikzExpr
tikzCrossover multi chain1 chrom1 chain2 chrom2 rs1 rs2 =
    tikzCrossover' chain1 (c chrom1) chain2 (c chrom2) [] (r rs1 cls1) [] (r rs2 cls2)
        where c (x,y) = ([],x,y)
              as = map (\(c,i) -> let as = ["yshift=" ++ show i ++ "pt" | multi]
                              in ("fill =" ++ lighter c 50):as
                       )
              r q = zipWith (\p a -> (a,p)) q . as . (`zip` [0..])

              cls1 = stdColors
              cls2 = if multi then stdColors else drop (length rs1) stdColors




