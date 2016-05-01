module Day4Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day4

main :: IO ()
main = hspec spec

input = "yzbqklnj"

spec :: Spec
spec = do

  describe "Trying to do part 1" $ do
    it "will mine the adventcoins at the correct difficulty of 5" $ do
      mineAdvent input 5 `shouldBe` 282749

  describe "Tring to do part 2" $ do
    it "will mine the adventcoins at the correct difficulty of 6" $ do
      mineAdvent input 6 `shouldBe` 9962624

