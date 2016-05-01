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

    it "visits houses from a file" $ do
      contents <- readFile "test/input-day3.txt"
      santaHouses contents `shouldBe` 2081

  describe "When the Robot and Santa are working together" $ do
    it "visits one house each for two diff instructions" $ do
      combinedHouses "><" `shouldBe` 3

    it "they visit their same houses but counts it once" $ do
      combinedHouses "^vv^" `shouldBe` 3

    it "they both visit the same house but counts it only once" $ do
      combinedHouses "^>>^" `shouldBe` 4

    it "visits 5 houses each" $ do
      combinedHouses "^v^v^v^v^v" `shouldBe` 11

    it "visits naughty houses" $ do
      combinedHouses ">>v^" `shouldBe` 4

    it "Visits houses form a file" $ do
      contents <- readFile "test/input-day3.txt"
      combinedHouses contents `shouldBe` 2341

