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
import Parcial2.Labyrinth (Route(..), POI(..), POIs(..))

import Test.Hspec
import Test.QuickCheck

import Data.List (sort)
import GHC.Exts (Down(..), sortWith)

-----------------------------------------------------------------------------

instance Arbitrary POI where arbitrary = toEnum <$> arbitrary
instance Arbitrary POIs where arbitrary = oneof [ elements [POINone, POIBoth]
                                                , POISome <$> arbitrary ]

-----------------------------------------------------------------------------



routeSpec = describe "Route" $ do

    specify "Any 'CompleteRoute' < any 'PartialRoute'" $ property $
        \x v i l -> (CompleteRoute x :: Route) `shouldSatisfy` (< PartialRoute v i l)

    it "compares 'CompleteRoute' by underlying value" $ property $
        \x y -> compare x y `shouldBe` compare (CompleteRoute x)
                                               (CompleteRoute y)
    it "compares 'PartialRoute' by underlying values in lexografical order,\
       \changing result to opposite" $ property $
        \p1@(v1,i1,l1) p2@(v2,i2,l2) -> compare p1 p2 `shouldBe` compare (PartialRoute v2 i2 l2)
                                                                         (PartialRoute v1 i1 l1)



cr = CompleteRoute
pr = PartialRoute

cr1 = cr 2
cr2 = cr 1
pr1 = pr 0.6 (POISome POIInit) 4
cr3 = cr 5
pr2 = pr 0.6 (POISome POITarget) 7
cr4 = cr 2
pr3 = pr 0.7 POINone 6

rEx1 = [cr1, cr2, pr1, cr3, pr2, cr4, pr3]

eEx1Min = [ cr2, cr1, cr4, cr3, pr3, pr2, pr1 ]


routeOrdSpec = describe "Route ordering examples" $ do
    it "sorts correctly in 'Min' direction"
        $ sort (rEx1 :: [Route]) `shouldBe` eEx1Min


