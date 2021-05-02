{-# LANGUAGE TypeApplications #-}

module VmSpec where

import Test.Hspec
import Parser
import Compile
import Data.Stack

spec :: Spec
spec = do
  describe "wollok VM" $ do
    it "" $ do
      let ast =
            [w|
                program x {
                    42
                }
            |]
      let bytecode = compile ast
      let estadoFinalVm = run bytecode estadoInicialVm

      (stackPeek $ vmStack estadoFinalVm) `shouldBe` Just (WInteger 42)
    it "" $ do
      let ast =
            [w|
                program x {
                    41 + 1
                }
            |]
      let bytecode = compile ast
      let estadoFinalVm = run bytecode estadoInicialVm

      (stackPeek $ vmStack estadoFinalVm) `shouldBe` Just (WInteger 42)
    it "" $ do
      let ast =
            [w|
                program x {
                    10
                    41 + 1
                }
            |]
      let bytecode = compile ast
      let estadoFinalVm = run bytecode estadoInicialVm

      (toList $ vmStack estadoFinalVm) `shouldBe` [WInteger 42, WInteger 10]


todoSpec :: IO ()
todoSpec = error "Missing implementation"