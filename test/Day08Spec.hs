module Day08Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 8" $ do
  it "Sample" $ do
    day08 day08TestInput `shouldBe` ["21", "27", "2"]

  it "Actual" $ do
    withFile
      "inputs/day08.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day08 actualInput `shouldBe` ["9725698", "154255165", "41082"]
      )