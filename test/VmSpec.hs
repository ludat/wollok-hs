{-# LANGUAGE TypeApplications #-}

module VmSpec where

import Test.Hspec
import Parser
import Compile
import Parser.AbsGrammar

spec :: Spec
spec = do
  describe "wollok VM" $ do
    it "pushes literals to the stack" $ do
      stackAfterExecuting
        [w|
            program x {
                42
            }
        |]
        `shouldBe` [WInteger 42]
    it "resolves + native method for numbers" $ do
      stackAfterExecuting
        [w|
            class Number {
                method +(other) native
            }
            program x {
                40 + 1
            }
        |]
        `shouldBe` [WInteger 41]
    it "treats + and - messages as different messages" $ do
      stackAfterExecuting
        [w|
            class Number {
                method -(other) native
            }
            program x {
                41 - 1
            }
        |]
        `shouldBe` [WInteger 40]
    it "leaves the result of the message send in the stack" $ do
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
    it "consumes the correct number of arguments from the stack during a message send" $ do
      stackAfterExecuting
        [w|
            class Number {
                method between(min, max) native
            }
            program x {
                10.between(1, 20)
            }
        |]
        `shouldBe` [WBoolean True]

stackAfterExecuting :: WFile -> [StackFrame]
stackAfterExecuting = toList . vmStack . run . compile

todoSpec :: IO ()
todoSpec = error "Missing implementation"