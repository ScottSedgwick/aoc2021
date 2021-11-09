module Main (main) where

import Test.Hspec
import Day25

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part1" $ do
    it "test case" $ pt1 [1721,979,366,299,675, 1456] `shouldBe` 514579
  describe "part2" $ do
    it "test case" $ pt2 [1721,979,366,299,675, 1456] `shouldBe` 241861950