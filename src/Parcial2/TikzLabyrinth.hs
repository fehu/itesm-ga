-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.TikzLabyrinth
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module Parcial2.TikzLabyrinth  where

import Parcial2.Labyrinth


import Control.Monad.State.Strict
import Control.Arrow

import Data.List (intercalate)
import qualified Data.Set as Set


-----------------------------------------------------------------------------

type Color = String
type ChromosomeExample = (Color, [(Point2D, Point2D)])

tikzLabyrinth :: Labyrinth2D -> [ChromosomeExample] -> [TikzAttr] -> String
tikzLabyrinth l exs attrs = intercalate "\n" . extract . tikzPicture [] $ [
    tikzStyles [ "point"   --> ["draw"]
               , "initial" --> ["draw", "circle", "fill=green!50", "inner sep=2pt"]
               , "target"  --> ["draw", "circle", "fill=blue!50", "inner sep=2pt"]
               ]
  , newline
  ]
  ++ map node (Set.toList $ nodes l)
  ++ [ newline
     , tikzScope [] $ map edge (Set.toList $ edges l)
     , newline
     ]
  ++ zipWith (chromosomeExample attrs) exs (reverse [1..length exs ])

  where node pnt@(Point2D (x,y)) =
            let clazz = case pnt of p | p == initial l -> "initial"
                                    p | p == target l  -> "target"
                                    _                  -> "point"
            in tikzNode [ clazz ]
                        (show pnt)
                        (Just $ AbsPos x y)
                        ("\\footnotesize " ++ show pnt)
        edge (x, y) = tikzEdge (show x) (show y) []

chromosomeExample attrs (color, es) w = tikzScope attrs' $ map route es
    where route (x, y) = tikzEdge (show x) (show y) []
          attrs' = [ "color=" ++ color
                   , "line width=" ++ show w ++ "pt"
                   ]
                  ++ attrs

-----------------------------------------------------------------------------

data TikzPos = AbsPos Int Int
             | TikzPos String

instance Show TikzPos where show (AbsPos x y) = show (x,y)
                            show (TikzPos s)  = s

type TikzAttr = String
type TikzBody = [String]

newtype TikzExpr = TikzExpr [String]
extract (TikzExpr es) = es

indent s = replicate 4 ' ' ++ s

newline = TikzExpr [""]

showAttrs [] = ""
showAttrs as = "[" ++ intercalate ", " as ++ "]"


tikzCmd :: String -> [TikzAttr] -> TikzBody -> TikzExpr
tikzCmd name as body = TikzExpr $ ('\\':name ++ showAttrs as ++ "{")
                                : map indent body
                               ++ ["}"]

tikzEnv :: String -> [TikzAttr] -> TikzBody -> TikzExpr
tikzEnv name as body = TikzExpr $ ("\\begin{" ++ name ++ "}" ++ showAttrs as )
                                : map indent body
                               ++ ["\\end{" ++ name ++ "}"]

tikzDef name body = TikzExpr ["\\def\\" ++ name ++ "{" ++ body ++ "}"]


tikzPicture :: [TikzAttr] -> [TikzExpr] -> TikzExpr
tikzPicture attrs body = tikzEnv "tikzpicture" attrs $ intercalate []
                                                     $ map extract body

a --> b = (a,b)

tikzStyles :: [(String, [TikzAttr])] -> TikzExpr
tikzStyles styles = TikzExpr $ "\\tikzset{" : map indent s ++ ["};"]
    where styles' :: [String]
          styles' = do (s, as) <- styles
                       [s ++ "/.style={ " ++ intercalate ", " as ++ " }"]
          s = if length styles' == 1
                then styles'
                else foldr (\s a -> a ++ [',':s]) [' ' :head styles'] (tail styles')

tikzNode :: [TikzAttr] -> String -> Maybe TikzPos -> String -> TikzExpr
tikzNode as id mbPos body = TikzExpr
  [ "\\node" ++ showAttrs as ++ " (" ++ id ++ ") " ++ pos ++ "{" ++ body ++ "};" ]
  where pos = maybe "" ((" at " ++) . show) mbPos

tikzEdge :: String -> String -> [TikzAttr] -> TikzExpr
tikzEdge n1 n2 as = TikzExpr [
    "\\draw (" ++ n1 ++ ") edge" ++ showAttrs as ++ " (" ++ n2 ++ ");"
  ]

tikzScope :: [TikzAttr] -> [TikzExpr] -> TikzExpr
tikzScope as body = tikzEnv "scope" as $ concatMap extract body


