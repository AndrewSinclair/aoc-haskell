module Day3Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day3

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "When Santa is alone" $ do
    it "visits only one house for one instruction" $ do
      santaHouses ">" `shouldBe` 2

    it "Visits houses form a file" $ do
      contents <- readFile "test/input-day3.txt"
      combinedHouses contents `shouldBe` 2341

