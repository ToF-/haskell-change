module ChangeSpec where

import Test.Hspec
import Change

spec :: SpecWith ()
spec = do
  describe "change computes how many ways to make change" $ do
    describe "for a single piece" $ do
      it "of same amount" $ do
        change 5 [5] `shouldBe` 1
        change 2 [2] `shouldBe` 1

      it "with incompatible amount" $ do
        change 2 [5] `shouldBe` 0

      it "with incompatible amount recursively" $ do
        change 7 [5] `shouldBe` 0

      it "with compatible amount recursively" $ do
        change 10 [5] `shouldBe` 1

    describe "for several pieces" $ do
      it "of non multiples" $ do
        change 10 [7,3] `shouldBe` 1 
        change 7  [7,3] `shouldBe` 1
      it "with incompatible amount" $ do
        change 8  [7,3] `shouldBe` 0

      it "with several solutions" $ do
        change 10 [5,2] `shouldBe` 2
        change 12 [5,2] `shouldBe` 2
        change 11 [5,1] `shouldBe` 3
        let result = fromIntegral $ length [[5,5],[5,2,2,1],[5,2,1,1,1],[5,1,1,1,1,1],[2,2,2,2,2],[2,2,2,2,1,1],[2,2,2,1,1,1,1],[2,2,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1]]
        change 10 [5,2,1] `shouldBe` result

      it "in any order of coin value" $ do
        change 10 [1,2,5] `shouldBe` 10
        change 10 [3,7] `shouldBe` 1
        change 300 [5,10,20,50,500,200,100] `shouldBe` 1022

