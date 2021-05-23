{-# LANGUAGE TypeApplications #-}

module VmSpec where

import Test.Hspec
import Parser
import Compile
import Parser.AbsGrammar

spec :: Spec
spec = do
  describe "wollok VM" $ do
    describe "literals" $ do
      it "pushes literals to the stack" $ do
        stackAfterExecuting
          [w|
              program x {
                  42
              }
          |]
          `shouldBe` [WInteger 42]
    describe "message sends" $ do
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
    describe "method definitions" $ do
      it "can execute user-defined methods defined by single expression" $ do
        stackAfterExecuting
          [w|
              class Number {
                  method twenty() = 20
              }
              program x {
                  10.twenty()
              }
          |]
          `shouldBe` [WInteger 20]
      it "can execute user-defined methods defined by block" $ do
        stackAfterExecuting
          [w|
              class Number {
                  method twenty() {
                    return 20
                  }
              }
              program x {
                  10.twenty()
              }
          |]
          `shouldBe` [WInteger 20]
      it "methods defined by block with no return, return null" $ do
        stackAfterExecuting
          [w|
              class Number {
                  method twenty() {
                    20
                  }
              }
              program x {
                  10.twenty()
              }
          |]
          `shouldBe` [WNull]
      it "self refers to the receiver of the message" $ do
        stackAfterExecuting
          [w|
              class Number {
                  method myself() = self
              }
              program x {
                  10.myself()
              }
          |]
          `shouldBe` [WInteger 10]
    describe "object initialization" $ do
      it "initializes objects without instance variables" $ do
        stackAfterExecuting
          [w|
              class Golondrina {
                  method energia() = 10
              }
              program x {
                  new Golondrina().energia()
              }
          |]
          `shouldBe` [WInteger 10]
      it "initializes objects with instance variables" $ do
        stackAfterExecuting
          [w|
              class Golondrina {
                  var energia = 10
                  method energia() = energia
              }
              program x {
                  new Golondrina().energia()
              }
          |]
          `shouldBe` [WInteger 10]
      it "initializes objects with multiple instance variables" $ do
        stackAfterExecuting
          [w|
              class Golondrina {
                  var energia = 10
                  var alegria = 23
                  method energia() = energia
                  method alegria() = alegria
              }
              program x {
                  new Golondrina().energia()
                  new Golondrina().alegria()
              }
          |]
          `shouldBe` [WInteger 23, WInteger 10]
      it "initializes objects with instance variables with constructor arguments" $ do
        stackAfterExecuting
          [w|
              class Golondrina {
                  var energia = 10
                  method energia() = energia
              }
              program x {
                  new Golondrina(energia = 4).energia()
              }
          |]
          `shouldBe` [WInteger 4]
      it "initializes objects with instance variables that have no default value \
         \with constructor arguments" $ do
        stackAfterExecuting
          [w|
              class Golondrina {
                  var energia
                  method energia() = energia
              }
              program x {
                  new Golondrina(energia = 4).energia()
              }
          |]
          `shouldBe` [WInteger 4]
      it "fails when an instance variable is not initialized after object creation" $ do
        pendingWith "tendriamos que implementar excepciones primero o atrapar \
                    \excepciones de haskell."
    describe "instance variables" $ do
      it "modifies instance variables when there's an assignment" $ do
        stackAfterExecuting
          [w|
              class Golondrina {
                  var energia
                  method energia() = energia
                  method cambiarEnergia() {
                      energia = 2
                  }
              }
              program x {
                  var golondrina = new Golondrina(energia = 4)
                  golondrina.cambiarEnergia()
                  golondrina.energia()
              }
          |]
          `shouldBe` [WInteger 2, WNull]
    describe "local variables" $ do
      it "can obtain the value of a local variable defined in the same context" $ do
        stackAfterExecuting
          [w|
              class Number {
                  method +(other) native
              }
              program x {
                  var n = 2
                  n + 1
              }
          |]
          `shouldBe` [WInteger 3]
      it "shadows instance variables when there is a local variable with the same name (read)" $ do
        stackAfterExecuting
          [w|
              class Golondrina {
                  var energia
                  method energia() {
                      var energia = 2
                      return energia
                  }
              }
              program x {
                  new Golondrina(energia = 4).energia()
              }
          |]
          `shouldBe` [WInteger 2]
      it "defines null as the value of uninitialized variables" $ do
        stackAfterExecuting
          [w|
              program x {
                  var y
                  y
              }
          |]
          `shouldBe` [WNull]
      it "fails when a const is defined with no value" $ do
        pendingWith "tendriamos que ver cómo hacer que el compilador notifique errores o atrapar \
                    \excepciones de haskell."
      it "fails when a const variable is assigned" $ do
        pendingWith "tendriamos que ver cómo hacer que el compilador notifique errores o atrapar \
                    \excepciones de haskell."
      it "const variables with an initial value can be used" $ do
        stackAfterExecuting
          [w|
              program x {
                  const y = 3
                  y
              }
          |]
          `shouldBe` [WInteger 3]
      it "changes the value of vars when an assignment is executed" $ do
        stackAfterExecuting
          [w|
              program x {
                  var y = 3
                  y = 4
                  y
              }
          |]
          `shouldBe` [WInteger 4]
      it "fails when a non-declared variable is assigned" $ do
        pendingWith "tendriamos que ver cómo hacer que el compilador notifique errores o atrapar \
                    \excepciones de haskell."
      it "shadows instance variables when there is a local variable with the same name (write)" $ do
        stackAfterExecuting
          [w|
              class Golondrina {
                  var energia
                  method energia() {
                      var energia = 2
                      energia = 3
                      return energia
                  }
              }
              program x {
                  new Golondrina(energia = 4).energia()
              }
          |]
          `shouldBe` [WInteger 3]
    describe "if" $ do
      it "executes the then branch if the condition evaluates to true" $ do
        stackAfterExecuting
          [w|
              program x {
                  if (true) {
                      1
                  } else {
                      0
                  }
              }
          |]
          `shouldBe` [WInteger 1]
      it "executes the else branch if the condition evaluates to false" $ do
        stackAfterExecuting
          [w|
              program x {
                  if (false) {
                      1
                  } else {
                      0
                  }
              }
          |]
          `shouldBe` [WInteger 0]
      it "works with ifs that have single statements as branches" $ do
        stackAfterExecuting
          [w|
              program x {
                  if (true) 1 else 0
              }
          |]
          `shouldBe` [WInteger 1]


stackAfterExecuting :: WFile -> [RuntimeValue]
stackAfterExecuting = concatMap (toList . valueStack) . toList . vmStack . run . compile

todoSpec :: IO ()
todoSpec = error "Missing implementation"