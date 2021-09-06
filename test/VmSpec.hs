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
              program x {
                  40 + 1
              }
          |]
          `shouldBe` [WInteger 41]
      it "treats + and - messages as different messages" $ do
        stackAfterExecuting
          [w|
              program x {
                  41 - 1
              }
          |]
          `shouldBe` [WInteger 40]
      it "leaves the result of the message send in the stack" $ do
        stackAfterExecuting
          [w|
              program x {
                  10
                  41 + 1
              }
          |]
          `shouldBe` [WInteger 42, WInteger 10]
      it "consumes the correct number of arguments from the stack during a message send for native methods" $ do
        stackAfterExecuting
          [w|
              program x {
                  10.between(1, 20)
              }
          |]
          `shouldBe` [WBoolean True]
      it "consumes the correct number of arguments from the stack during a message send \
         \ for non-native methods" $ do
        stackAfterExecuting
          [w|
              class Cosa {
                  method first(a, b) {
                      return a
                  }
              }
              program x {
                  new Cosa().first(1, 2)
              }
          |]
          `shouldBe` [WInteger 1]
    describe "method definitions" $ do
      it "can execute user-defined methods defined by single expression" $ do
        stackAfterExecuting
          [w|
              class Coso {
                  method twenty() = 20
              }
              program x {
                  new Coso().twenty()
              }
          |]
          `shouldBe` [WInteger 20]
      it "can execute user-defined methods defined by block" $ do
        stackAfterExecuting
          [w|
              class Coso {
                  method twenty() {
                    return 20
                  }
              }
              program x {
                  new Coso().twenty()
              }
          |]
          `shouldBe` [WInteger 20]
      it "methods defined by block with no return, return null" $ do
        stackAfterExecuting
          [w|
              class Coso {
                  method twenty() {
                    20
                  }
              }
              program x {
                  new Coso().twenty()
              }
          |]
          `shouldBe` [WNull]
      it "self refers to the receiver of the message" $ do
        stackAfterExecuting
          [w|
              class Coso {
                  method myself() = self
                  method ten() = 10
              }
              program x {
                  new Coso().myself().ten()
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
                      return self.energia()
                  }
              }
              program x {
                  var golondrina = new Golondrina(energia = 4)
                  golondrina.cambiarEnergia()
              }
          |]
          `shouldBe` [WInteger 2]

      it "instance variables changes persist across references" $ do
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
    describe "closures" $ do
      it "calling a closure executes its body" $ do
        stackAfterExecuting
          [w|
              program x {
                  { return 7 }.apply() + 1
              }
          |]
          `shouldBe` [WInteger 8]
      it "executing a closure returns the result of the last statement" $ do
        stackAfterExecuting
          [w|
              program x {
                  { 7 }.apply() + 1
              }
          |]
          `shouldBe` [WInteger 8]
      it "closures capture self" $ do
        stackAfterExecuting
          [w|
              class Coso {
                method myself() {
                  return { self }
                }
                method m1() {
                  return 7
                }
              }
              program x {
                  new Coso().myself().apply().m1()
              }
          |]
          `shouldBe` [WInteger 7]
      it "variables declared inside a closure are local to that closure" $ do
        stackAfterExecuting
          [w|
              class Coso {
                method m1() {
                  var x = 1
                  {
                      var x = 2
                  }.apply()

                  return x
                }
              }
              program x {
                  new Coso().m1()
              }
          |]
          `shouldBe` [WInteger 1]
      it "variables from the parent context can be set from a closure" $ do
        stackAfterExecuting
          [w|
              class Coso {
                method m1() {
                  var x = 1
                  { =>
                    x = 2
                  }.apply()

                  return x
                }
              }
              program x {
                  new Coso().m1()
              }
          |]
          `shouldBe` [WInteger 2]
      it "variables from the parent context can be read from a closure" $ do
        stackAfterExecuting
          [w|
              class Coso {
                method m1() {
                  var x = 1
                  return { => x }.apply()
                }
              }
              program x {
                  new Coso().m1()
              }
          |]
          `shouldBe` [WInteger 1]
      it "returns from a closure are local" $ do
        stackAfterExecuting
          [w|
              class Coso {
                method m1() {
                  let x = { return 1 }.apply()
                  return x + 2
                }
              }
              program x {
                  new Coso().m1()
              }
          |]
          `shouldBe` [WInteger 3]
      it "try statements without throws doesn't run catch blocks" $ do
        stackAfterExecuting
          [w|
              program x {
                  try {
                    0
                  } catch e {
                    1
                  }
              }
          |]
          `shouldBe` [WInteger 0]
      fit "" $ do
        stackAfterExecuting
          [w|
              program x {
                  try {
                    throw new Exception()
                    0
                  } catch e {
                    1
                  }
              }
          |]
          `shouldBe` [WInteger 1]

stackAfterExecuting :: WFile -> [RuntimeValue]
stackAfterExecuting (WFile imports libraryElements program) =
  let
      programWithStandardLibrary = WFile imports (standardLibrary ++ libraryElements) program
      [topStackFrame] = toList . vmStack . run . compile $ programWithStandardLibrary
  in toList . valueStack $ topStackFrame

standardLibrary :: [WLibraryElement]
standardLibrary =
  case [w|
    class Closure {
      method apply() native
    }
    class Number {
      method +(a) native
      method -(a) native
      method between(a, b) native
    }
    class Boolean {}
    class Exception {}
    program x {}
  |] of
    WFile _ libraryElements _ -> libraryElements


todoSpec :: IO ()
todoSpec = error "Missing implementation"