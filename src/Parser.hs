{-# LANGUAGE DeriveAnyClass #-}
module Parser where

import Parser.ParGrammar (myLexer, pWFile)
import qualified Parser.AbsGrammar
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

parsearWollok :: String -> Either String Parser.AbsGrammar.WFile
parsearWollok = pWFile . myLexer

w :: QuasiQuoter
w =
  QuasiQuoter
    { quoteExp = wollokQuote
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
  where
    wollokQuote :: String -> Q Exp
    wollokQuote s = do
      case parsearWollok s of
        (Left err) -> fail err
        (Right ast) -> liftData ast

