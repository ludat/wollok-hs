{-# LANGUAGE TypeApplications #-}

module ParserSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "xxx" $ do
    it "aaa" $ do
      False `shouldBe` False