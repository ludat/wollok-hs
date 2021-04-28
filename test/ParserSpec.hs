{-# LANGUAGE TypeApplications #-}

module ParserSpec where

import Test.Hspec
import Parser
import Parser.AbsGrammar
import Text.InterpolatedString.Perl6 (q)

spec :: Spec
spec = do
  describe "wollok parser" $ do
    it "should parse wollok" $ do
      [q|
        class X {
          var v1 = 2
          method m1(x, y) {
            x
          }
          method m2() native
        }
        class Y inherits X {}

        program testExpression {
          var x = 42
          const nada = null
          true
          false
          "un string"
          x.abs().sumarCon(1, 2)
        }
      |] `shouldParseAs`
        (WFile []
          [ WLibraryElement $ WClassDeclaration (Ident "X") WNoSuperclass
            [ WVariableDeclaration Var (Ident "v1") (WNumberLiteral 2) ]
            [ WMethodDeclaration (Ident "m1") ["x", "y"] Custom
                $ Implemented [ TopLevelExpression $ WVariable "x" ]
            , WMethodDeclaration (Ident "m2") [] Native NotImplemented
            ]
          , WLibraryElement $ WClassDeclaration (Ident "Y") (WSuperclass (Ident "X")) [] []
          ]
          (WProgram "testExpression"
            [ VarDeclaration $ WVariableDeclaration Var "x" $ WNumberLiteral 42
            , VarDeclaration $ WVariableDeclaration Const "nada" $ WNullLiteral
            , TopLevelExpression WLiteralTrue
            , TopLevelExpression WLiteralFalse
            , TopLevelExpression $ WStringLiteral "un string"
            , TopLevelExpression $
                WMessageSend
                  (WMessageSend (WVariable "x") "abs" []) "sumarCon" [
                    WNumberLiteral 1, WNumberLiteral 2
                  ]
            ]
          )
        )

shouldParseAs :: String -> WFile -> Expectation
shouldParseAs string expectedResult = parsearWollok' string `shouldBe` pure expectedResult
