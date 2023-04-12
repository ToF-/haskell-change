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


