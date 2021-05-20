{-# LANGUAGE TypeApplications #-}

module ParserSpec where

import Test.Hspec
import Parser
import Parser.AbsGrammar

spec :: Spec
spec = do
  describe "wollok parser" $ do
    it "should parse wollok" $ do
      [w|
        // This is a comment
        /* This is another comment */
        class X {
          var v1 = 2
          method m1(x, y) {
            x
          }
          method m2() native
          method m3() = 2
        }
        class Y inherits X {}

        program testExpression {
          var x = 42
          const nada = null
          true
          false
          "un string"
          x.abs().sumarCon(1, 2)

          try throw 2 catch e : Exception {
            return 3
          }
          not true || 3 <= 4 && 5 > 6 + 7 + 8 * 9 ** 10 ** 11
          return if (a == 7) {
            42
          } else {
            13
          }
          { a, b => return 7 }
          { => return 7 }
          { return 7 }
          object console {
            method println(obj) native
          }
          new X()
          new X(var1 = 1, var2 = 1 + 1)
          new X().m1()
        }
      |] `shouldBe`
        (WFile []
          [ WLibraryElement $ WClassDeclaration (Ident "X") WNoSuperclass
            [ WVariableDeclaration Var (Ident "v1") $ WithInitialValue (WNumberLiteral 2) ]
            [ WMethodDeclaration (WSelector $ Ident "m1") ["x", "y"]
                $ ImplementedByBlock [ TopLevelExpression $ WVariable "x" ]
            , WMethodDeclaration (WSelector $ Ident "m2") [] ImplementedNatively
            , WMethodDeclaration (WSelector $ Ident "m3") [] (ImplementedByExpression $ WNumberLiteral 2)
            ]
          , WLibraryElement $ WClassDeclaration (Ident "Y") (WSuperclass (Ident "X")) [] []
          ]
          (WProgram "testExpression"
            [ VarDeclaration $ WVariableDeclaration Var "x" $ WithInitialValue $ WNumberLiteral 42
            , VarDeclaration $ WVariableDeclaration Const "nada" $ WithInitialValue $ WNullLiteral
            , TopLevelExpression WLiteralTrue
            , TopLevelExpression WLiteralFalse
            , TopLevelExpression $ WStringLiteral "un string"
            , TopLevelExpression $
                WMessageSend
                  (WMessageSend (WVariable "x") "abs" []) "sumarCon" [
                    WNumberLiteral 1, WNumberLiteral 2
                  ]
            , TopLevelExpression $
                WTry
                  (SingleExpression (WThrow $ WNumberLiteral 2))
                    [ WCatch (Ident "e") (ProvidedExceptionType (Ident "Exception"))
                        (Block [ WReturn $ WNumberLiteral  3 ])
                    ] WNoThenAlways
            , TopLevelExpression (
                WOrExpression (WUnaryExpression OpUnary_not WLiteralTrue) OpOr1
                  (WAndExpression
                    (WCmpExpression (WNumberLiteral 3) OpCmp2 (WNumberLiteral 4)) OpAnd1
                      (WCmpExpression (WNumberLiteral 5) OpCmp3
                        (WAddExpression (WAddExpression (WNumberLiteral 6) OpAdd1 (WNumberLiteral 7)) OpAdd1
                          (WMultExpression (WNumberLiteral 8) OpMult1
                            (WPowerExpression (WNumberLiteral 9) OpPower1
                              (WPowerExpression (WNumberLiteral 10) OpPower1 (WNumberLiteral 11))))))))
            , WReturn (
              WIf (WEqExpression (WVariable (Ident "a")) OpEq1 (WNumberLiteral 7))
                (Block [TopLevelExpression $ WNumberLiteral 42]) (WElse (Block [TopLevelExpression (WNumberLiteral 13)])))
            , TopLevelExpression $
                WClosure (WWithParameters [Ident "a", Ident "b"]) [WReturn $ WNumberLiteral 7]
            , TopLevelExpression $
                WClosure (WWithParameters []) [WReturn $ WNumberLiteral 7]
            , TopLevelExpression $
                WClosure WNoParameters [WReturn $ WNumberLiteral 7]
            , TopLevelExpression (WObjectLiteral (Ident "console") WNoSuperclass []
                [ WMethodDeclaration (WSelector $ Ident "println") [Ident "obj"] ImplementedNatively])
            , TopLevelExpression (WNew (Ident "X") [])
            , TopLevelExpression (WNew (Ident "X")
              [ WNewParameter (Ident "var1") (WNumberLiteral 1)
              , WNewParameter (Ident "var2") (WAddExpression (WNumberLiteral 1) OpAdd1 (WNumberLiteral 1))
              ])
            , TopLevelExpression (WMessageSend (WNew (Ident "X") []) (Ident "m1") [])
            ]
          )
        )
