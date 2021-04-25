module Parser where

import Language.JavaScript.Inline
import Data.Aeson
import GHC.Generics
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LBS
import AST
import Paths_wollok

data WollokParsingException
  = WollokParsingException
  { exception :: String
  , stack :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

parsearWollok :: String -> LBS.ByteString -> IO (Either WollokParsingException WollokAST)
parsearWollok fileName fileContent = do
  wollokTsPath <- getDataFileName "wollok-ts/dist/index.js"

  let
      encodedFilename = EncodedString $ LBS.pack fileName
      encodedContent = EncodedString $ fileContent
      encodedWollokTsPath = EncodedString $ LBS.pack wollokTsPath

  EncodedJSON resultado <- withSession defaultConfig $ \s -> eval @EncodedJSON s
    [js|
        try {
          const wollokTs = require($encodedWollokTsPath);

          return wollokTs.parse.File($encodedFilename).tryParse($encodedContent);
        } catch (e) {
          return {exception: e.message, stack: e.stack};
        }
    |]

--   case fmap encodePretty $ eitherDecode' @Value $ resultado of
--       Right bs -> LBS.putStrLn bs
--       Left e -> putStrLn e

  case eitherDecode' @WollokParsingException $ resultado of
    Right (WollokParsingException {..}) -> do
        putStrLn exception
        putStrLn stack
        return $ Left WollokParsingException {..}
    Left e ->
      case eitherDecode' @WollokAST $ resultado of
        Right bs -> return $ Right bs
        Left e -> do
          error $ e
