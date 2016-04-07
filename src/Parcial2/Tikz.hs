-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.Tikz
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleInstances #-}

module Parcial2.Tikz where

import Data.List (intercalate)
import qualified Data.Set as Set


-----------------------------------------------------------------------------

strSurround l r s = l ++ s ++ r
strBrace = strSurround "{" "}"
strSquare = strSurround "[" "]"
strRound = strSurround "(" ")"

-----------------------------------------------------------------------------


data TikzPos = AbsPos Int Int
             | TikzPos String

instance Show TikzPos where show (AbsPos x y) = show (x,y)
                            show (TikzPos s)  = s

type TikzAttr = String
type TikzBody = [String]

newtype TikzExpr = TikzExpr [String]
extract (TikzExpr es) = es

instance Show TikzExpr where show = intercalate "\n" . extract

indent s = replicate 4 ' ' ++ s

newline = TikzExpr [""]

showAttrs [] = ""
showAttrs as = strSquare $ intercalate ", " as


tikzCmd :: String -> [TikzAttr] -> TikzBody -> TikzExpr
tikzCmd name as body = TikzExpr $ ('\\':name ++ showAttrs as ++ "{")
                                : map indent body
                               ++ ["}"]

tikzEnv :: String -> [TikzAttr] -> TikzBody -> TikzExpr
tikzEnv name as body = TikzExpr $ ("\\begin{" ++ name ++ "}" ++ showAttrs as )
                                : map indent body
                               ++ ["\\end{" ++ name ++ "}"]

-----------------------------------------------------------------------------

tikzDef name body = TikzExpr ["\\def\\" ++ name ++ strBrace body ]
tikzEDef name body = TikzExpr ["\\edef\\" ++ name ++ strBrace body ]


tikzPicture :: [TikzAttr] -> [TikzExpr] -> TikzExpr
tikzPicture attrs body = tikzEnv "tikzpicture" attrs $ intercalate []
                                                     $ map extract body


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
  [ "\\node" ++ showAttrs as ++ " (" ++ id ++ ") " ++ pos ++ strBrace body ++ ";" ]
  where pos = maybe "" ((" at " ++) . show) mbPos

tikzNode' :: [TikzAttr] -> String -> TikzExpr
tikzNode' as body = TikzExpr
  [ "\\node" ++ showAttrs as ++ strBrace body ++ ";" ]

tikzEdge :: String -> String -> [TikzAttr] -> TikzExpr
tikzEdge n1 n2 as = TikzExpr [
    "\\draw (" ++ n1 ++ ") edge" ++ showAttrs as ++ strRound n2 ++ ";"
  ]

tikzScope :: [TikzAttr] -> [TikzExpr] -> TikzExpr
tikzScope as body = tikzEnv "scope" as $ concatMap extract body

-----------------------------------------------------------------------------

a --> b = (a,b)

lighter c prc = c ++ "!" ++ show prc

stdColors = [ "blue"
            , "brown"
            , "cyan"
            , "green"
            , "lime"
            , "magenta"
            , "olive"
            , "orange"
            , "pink"
            , "purple"
            , "red"
            , "teal"
            , "violet"
            , "yellow"
            , "darkgray"
            , "gray"
            , "lightgray"
            , "black"
            , "white"
            ]



