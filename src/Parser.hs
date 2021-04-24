module Parser where

import Language.JavaScript.Inline
import Data.Aeson
import GHC.Generics
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LBS
import AST

data WollokParsingException
  = WollokParsingException
  { exception :: String
  , stack :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

parsearWollok :: String -> LBS.ByteString -> IO (Either WollokParsingException WollokAST)
parsearWollok fileName fileContent = do
  let
      encodedFilename = EncodedString $ LBS.pack fileName
      encodedContent = EncodedString $ fileContent

  EncodedJSON resultado <- withSession defaultConfig $ \s -> eval @EncodedJSON s
    [js|
        try {
          const wollok = require('/home/ludat/Projects/curso-vms/wollok/wollok-ts/dist/index.js');
          return wollok.parse.File($encodedFilename).tryParse($encodedContent);
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
          error $ toText e
