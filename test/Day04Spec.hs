module Day04Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 4" $ do
  it "Sample" $ do
    day04 day04TestInput `shouldBe` ["10", "10", "8"]

  it "Actual" $ do
    withFile
      "inputs/day04.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day04 actualInput `shouldBe` ["67", "898681", "124770256"]
      )