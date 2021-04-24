module AST where

import Data.Aeson
import Data.Text (Text)
import Data.Scientific
import GHC.Generics
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data WollokAST
  = Package { packageMembers :: [WollokAST], packageName :: String, packageImports :: [()] }
  | Class { classMembers :: [WollokAST], className :: String, classMixins :: [()] }
  | Method { methodBody :: WollokAST, methodName :: String, methodParameters :: [String], methodIsOverride :: Bool }
  | Body { bodySentences :: [WollokAST] }
  | Return { returnValue :: WollokAST }
  | LiteralNumber { literalNumberValue :: Scientific }
  | LiteralString { literalStringValue :: Text }
  | LiteralNull
  | Literal { literalValue :: WollokAST }
  | Reference { referenceName :: String }
  | Variable { variableValue :: WollokAST, variableIsReadOnly :: Bool, variableName :: String }
  | Assignment { assignmentValue :: WollokAST, assignmentVariable :: WollokAST }
  | New { newInstantiated :: WollokAST, newArgs :: [WollokAST]}
  | Send { sendReceiver :: WollokAST, sendArgs :: [WollokAST], sendMessage :: String}
  | Field { fieldName :: String, fieldValue :: WollokAST, fieldIsReadOnly :: Bool, fieldIsProperty :: Bool}
  | Singleton
    { singletonSupercallArgs :: [WollokAST]
    , singletonMixins :: [WollokAST]
    , singletonMembers :: [WollokAST]
    , singletonSuperclassRef :: WollokAST
    , singletonName :: String
    }
  | Program { programBody :: WollokAST, programName :: String }
  deriving (Show, Generic)

instance FromJSON WollokAST where
  parseJSON = withObject "wollokast" $ \o -> do
    kind <- o .: "kind"
    case (kind :: String) of
      "Package" -> do
          packageMembers <- o .: "members"
          packageName <- o .: "name"
          packageImports <- o .: "imports"
          return Package {..}
      "Class" -> do
          classMembers <- o .: "members"
          className <- o .: "name"
          classMixins <- o .: "mixins"
          return Class {..}
      "Method" -> do
          methodBody <- o .: "body"
          methodName <- o .: "name"
          methodParameters <- o .: "parameters"
          methodIsOverride <- o .: "isOverride"
          return Method {..}
      "Body" -> do
          bodySentences <- o .: "sentences"
          return Body {..}
      "Return" -> do
          returnValue <- o .:? "value" .!= LiteralNull
          return Return {..}
      "Literal" -> do
          v <- o .: "value"
          case v of
            Number n -> return $ LiteralNumber n
            String t -> return $ LiteralString t
            Null -> return $ LiteralNull
            _ -> do
              literalValue <- o .: "value"
              return Literal {..}
      "Reference" -> do
          referenceName <- o .: "name"
          return Reference {..}
      "Variable" -> do
          variableValue <- o .: "value"
          variableName <- o .: "name"
          variableIsReadOnly <- o .: "isReadOnly"
          return Variable {..}
      "Assignment" -> do
          assignmentValue <- o .: "value"
          assignmentVariable <- o .: "variable"
          return Assignment {..}
      "New" -> do
          newInstantiated <- o .: "instantiated"
          newArgs <- o .: "args"
          return New {..}
      "Send" -> do
          sendReceiver <- o .: "receiver"
          sendArgs <- o .: "args"
          sendMessage <- o .: "message"
          return Send {..}
      "Singleton" -> do
          singletonSupercallArgs <- o .: "supercallArgs"
          singletonMixins <- o .: "mixins"
          singletonMembers <- o .: "members"
          singletonSuperclassRef <- o .: "superclassRef"
          singletonName <- o .:? "name" .!= ""
          return Singleton {..}
      "Program" -> do
          programBody <- o .: "body"
          programName <- o .: "name"
          return Program {..}
      "Field" -> do
          fieldValue <- o .: "value"
          fieldIsReadOnly <- o .: "isReadOnly"
          fieldIsProperty <- o .: "isProperty"
          fieldName <- o .: "name"
          return Field {..}
      _ -> fail $ "Kind '" ++ kind ++ "' no encontrado. " ++ (LBS.unpack $ encodePretty $ Object o)
