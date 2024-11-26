module Day14Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    day14 day14TestInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day14.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day14 actualInput `shouldBe` []
      )