module Day5Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Day5

main :: IO ()
main = hspec spec

filename = "test/input-day5.txt"

spec :: Spec
spec = do

  describe "The the condition for having 3 or more vowels" $ do
    it "returns false for a string of consenents" $ do
      hasVowels "bcdfg" `shouldBe` False

    it "returns false for an empty string" $ do
      hasVowels "" `shouldBe` False

    it "returns false for a string with exactly 2 vowels" $ do
      hasVowels "abcde" `shouldBe` False

    it "returns true for a string with exactly 3 vowels" $ do
      hasVowels "abcdefghij" `shouldBe` True

    it "returns true for a string with more than 3 vowels" $ do
      hasVowels "abcdefghijklmnopqrstuvwxyz" `shouldBe` True
      
  describe "The condition for having double letters" $ do
    it "returns false for an empty string" $ do
      hasDoubles "" `shouldBe` False

    it "returns false for a string with one letter" $ do
      hasDoubles "a" `shouldBe` False

    it "returns true for a string with just the same letter twice" $ do
      hasDoubles "aa" `shouldBe` True

    it "returns false for a string with two different letters" $ do
      hasDoubles "ab" `shouldBe` False

    it "returns false for a string with lots of letters, all unique" $ do
      hasDoubles "abcdefghijkl" `shouldBe` False

    it "returns false for a string with some duplicate letters, but no pairs are adjacent" $ do
      hasDoubles "abcdefgfcdeba" `shouldBe` False

    it "returns true for a string with a duplicate in the middle" $ do
      hasDoubles "abcdeffedcba" `shouldBe` True

    it "returns true for a string with a duplicate at the beginning" $ do
      hasDoubles "aabcdefg" `shouldBe` True

    it "returns true for a string with a duplicate at the end" $ do
      hasDoubles "abcdefgg" `shouldBe` True

    it "returns true for a string with more than one adjacent duplicate" $ do
      hasDoubles "abccdeffg" `shouldBe` True

  describe "Forbidden digraph condition" $ do
    it "return true if one or more of the forbidden digraphs exist in a string" $ do
      containsForbidden "zabrts" `shouldBe` True

    it "returns false if non of the forbidden digraphs exist in a string" $ do
      containsForbidden "alphanumeric" `shouldBe` False

    it "returns false if the string is empty" $ do
      containsForbidden "" `shouldBe` False

  describe "Solver for the complete algorithm for part 1" $ do
    it "solves part 1 on the file inputs" $ do
      contents <- readFile filename
      let inputs = lines contents
      countNice inputs `shouldBe` 236

  describe "The complete algorithm for part 2" $ do
    it "returns the answer to part 2 if given the file input" $ do
      contents <- readFile filename
      let inputs = lines contents
      countNiceBetter inputs `shouldBe` 51
      

