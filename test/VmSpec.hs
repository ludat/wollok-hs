{-# LANGUAGE TypeApplications #-}

module VmSpec where

import Test.Hspec
import Parser
import Compile
import Parser.AbsGrammar

spec :: Spec
spec = do
  describe "wollok VM" $ do
    it "" $ do
      stackAfterExecuting
        [w|
            program x {
                42
            }
        |]
        `shouldBe` [WInteger 42]
    it "" $ do
      stackAfterExecuting
        [w|
            class Number {
                method +(other) native
            }
            program x {
                41 + 1
            }
        |]
        `shouldBe` [WInteger 42]
    it "" $ do
      stackAfterExecuting
        [w|
            class Number {
                method +(other) native
            }
            program x {
                10
                41 + 1
            }
        |]
        `shouldBe` [WInteger 42, WInteger 10]

stackAfterExecuting :: WFile -> [StackFrame]
stackAfterExecuting = toList . vmStack . run . compile

todoSpec :: IO ()
todoSpec = error "Missing implementation"