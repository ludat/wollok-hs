module Lib where

import Data.Aeson
import GHC.Generics
import Language.JavaScript.Inline
import Text.Pretty.Simple
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Aeson.Encode.Pretty (encodePretty)
import AST
import Parser
import Text.InterpolatedString.Perl6 (q)

ejecutarWollokPonele :: IO ()
ejecutarWollokPonele = do
  putStrLn "#######################################################################################"
  content <- BS.readFile "main.wk"
  parsearWollok "archivo.wk" content
    >>= \case
          Right bs -> pPrintForceColor bs
          Left e -> pPrintForceColor e

