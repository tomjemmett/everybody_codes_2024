module Day10Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
    day10 day10TestInput `shouldBe` ["PTBVRCZHFLJWGMNS", "1851", "3889"]

  it "Actual" $ do
    withFile
      "inputs/day10.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day10 actualInput `shouldBe` ["XHRFNBMZTSVQWJLP", "196798", "208713"]
      )