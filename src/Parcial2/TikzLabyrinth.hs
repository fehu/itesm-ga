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
type ChromosomeExample = (Color, [Point2D])

tikzLabyrinth :: Labyrinth2D -> [ChromosomeExample] -> String
tikzLabyrinth l exs = intercalate "\n" . extract . tikzPicture [] $ [
    tikzStyles [ "point" --> ["draw", "circle", "inner sep=2pt"] ]
  , newline
  ]
  ++ map node (Set.toList $ nodes l)
  ++ [ newline
     , tikzScope [] $ map edge (Set.toList $ edges l)
     , newline
     ]
  ++ map chromosomeExample exs

  where node p@(Point2D (x,y)) = tikzNode ["point"]
                                          (show p)
                                          (Just $ AbsPos x y)
                                          ("\\footnotesize " ++ show p)
        edge (x, y) = tikzEdge (show x) (show y) []

chromosomeExample (color, c) = tikzScope ["draw opacity=0.5", "color=" ++ color]
                             . map route $ pairs c
    where pairs (f:s:t) = (f,s) : pairs (s:t)
          pairs _       = []
          route (x, y) = tikzEdge (show x) (show y) []

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
tikzStyles styles = tikzCmd "tikzset" [] styles'
    where styles' = do (s, as) <- styles
                       [s ++ "/.style={ " ++ intercalate ", " as ++ " }"]

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


