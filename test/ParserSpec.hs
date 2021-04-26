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
        program testExpression {
          42
        }
      |] `shouldParseAs`
        (WFile [] [] (WProgram "testExpression" [WNumberLiteral 42]))

shouldParseAs :: String -> WFile -> Expectation
shouldParseAs string expectedResult = parsearWollok' string `shouldBe` pure expectedResult
