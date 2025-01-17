module Day11Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
    day11 day11TestInput `shouldBe` ["8", "1", "268815"]

  it "Actual" $ do
    withFile
      "inputs/day11.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day11 actualInput `shouldBe` ["34", "221560", "1570268426482"]
      )