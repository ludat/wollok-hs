{-# LANGUAGE TypeApplications #-}

module VmSpec where

import Test.Hspec
import Parser
import Parser.AbsGrammar
import Compile
import Data.Stack

spec :: Spec
spec = do
  describe "wollok VM" $ do
    it "" $ do
        let ast = [w|
            program x {
                42
            }
        |]
        let bytecode = compile ast
        let estadoFinalVm = run bytecode estadoInicialVm

        (stackPeek $ vmStack estadoFinalVm) `shouldBe` Just (Value (WInteger 42))


todoSpec :: IO ()
todoSpec = error "Missing implementation"