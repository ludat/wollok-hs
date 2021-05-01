{-# LANGUAGE TypeApplications #-}

module VmSpec where

import Test.Hspec
import Parser
import Parser.AbsGrammar
import Compile

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
        let estadoInicialVm = undefined
        let estadoFinalVm = run bytecode estadoInicialVm

        -- estadoFinalVm.pila.top == WollokVMInteger 42
        todoSpec


todoSpec :: IO ()
todoSpec = error "Missing implementation"