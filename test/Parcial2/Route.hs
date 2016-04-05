-----------------------------------------------------------------------------
--
-- Module      :  Parcial2.Route
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module Parcial2.Route( routeSpec, routeOrdSpec ) where

import GeneticAlgorithm
import Parcial2.Labyrinth

import Test.Hspec
import Test.QuickCheck

import Data.List (sort)
import GHC.Exts (Down(..), sortWith)

-----------------------------------------------------------------------------

routeSpec = describe "Route" $ do

    context "direction set to 'Min'" $ do
        specify "forall 'Length' < forall 'Validess'" $ property $
            \x y -> (RouteLength x :: Route Min) `shouldSatisfy` (< RouteValidess y)

        it "compares 'Length's underlying values normally" $ property $
            \x y -> compare x y `shouldBe` compare (RouteLength x :: Route Min)
                                                   (RouteLength y)
        it "compares 'Validess' underlying values normally" $ property $
            \x y -> compare x y `shouldBe` compare (RouteValidess x :: Route Min)
                                                   (RouteValidess y)

    context "direction set to 'Max'" $ do
        specify "forall 'Length' > forall 'Validess'" $ property $
            \x y -> (RouteLength x :: Route Max) `shouldSatisfy` (> RouteValidess y)

        it "compares 'Length's underlying values normally" $ property $
            \x y -> compare x y `shouldBe` compare (RouteLength x :: Route Max)
                                                   (RouteLength y)
        it "compares 'Validess' underlying values normally" $ property $
            \x y -> compare x y `shouldBe` compare (RouteValidess x :: Route Max)
                                                   (RouteValidess y)


rl = RouteLength
rv = RouteValidess

rEx1 = [rl 2, rl 1, rv 0.6, rl 5, rv 0.9, rl 2, rv 0.2]

eEx1Min = [ rl 1, rl 2, rl 2, rl 5, rv 0.2, rv 0.6, rv 0.9 ]
eEx1Max = [ rl 5, rl 2, rl 2, rl 1, rv 0.9, rv 0.6, rv 0.2 ]


routeOrdSpec = describe "Route ordering examples" $ do
    it "sorts correctly in 'Min' direction"
        $ sort (rEx1 :: [Route Min]) `shouldBe` eEx1Min
    it "sorts Down correctly in 'Max' direction"
        $ sortWith Down (rEx1 :: [(Route Max)]) `shouldBe` eEx1Max


