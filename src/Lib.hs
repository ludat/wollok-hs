module Lib where

import Data.Aeson
import GHC.Generics
import Text.Pretty.Simple
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Aeson.Encode.Pretty (encodePretty)
import Parser
import Text.InterpolatedString.Perl6 (q)

run :: IO ()
run = undefined
