{-# OPTIONS_GHC -w #-}
-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.AbsGrammar where

import Prelude (Char, Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

data WFile = WFile [Import] [WLibraryElement] WProgram
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Import = Import
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WLibraryElement = WLibraryElement WClassDeclaration
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WProgram = WProgram Ident [WStatement]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WClassDeclaration
    = WClassDeclaration Ident WSuperclassDeclaration [WVariableDeclaration] [WMethodDeclaration]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WSuperclassDeclaration = WSuperclass Ident | WNoSuperclass
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WMethodDeclaration
    = WMethodDeclaration WSelector [Ident] MethodBody
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WSelector
    = WSelector Ident
    | WAddOpSelector OpAdd
    | WOrOpSelector OpOr
    | WAndOpSelector OpAnd
    | WMultOpSelector OpMult
    | WEqOpSelector OpEq
    | WCmpOpSelector OpCmp
    | WPowerOpSelector OpPower
    | WUnaryOpSelector OpUnary
    | WPostfixOpSelector OpPostfix
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data MethodBody
    = ImplementedByBlock [WStatement]
    | ImplementedByExpression WExpression
    | ImplementedNatively
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WStatement
    = TopLevelExpression WExpression
    | VarDeclaration WVariableDeclaration
    | WReturn WExpression
    | WThrow WExpression
    | WAssignment Ident WExpression
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WVariableDeclaration
    = WVariableDeclaration WVariableType Ident WExpression
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WVariableType = Var | Const
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WBlockOrExpression
    = SingleExpression WStatement | Block [WStatement]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WExpression
    = WTry WBlockOrExpression [WCatch] WThenAlways
    | WOrExpression WExpression OpOr WExpression
    | WAndExpression WExpression OpAnd WExpression
    | WEqExpression WExpression OpEq WExpression
    | WCmpExpression WExpression OpCmp WExpression
    | WAddExpression WExpression OpAdd WExpression
    | WMultExpression WExpression OpMult WExpression
    | WPowerExpression WExpression OpPower WExpression
    | WUnaryExpression OpUnary WExpression
    | WPostfixExpression WExpression OpPostfix
    | WMessageSend WExpression Ident [WExpression]
    | WClosure WClosureParameters [WStatement]
    | WIf WExpression WBlockOrExpression WElse
    | WObjectLiteral Ident WSuperclassDeclaration [WVariableDeclaration] [WMethodDeclaration]
    | WNew Ident [WNewParameter]
    | WNumberLiteral Integer
    | WNullLiteral
    | WLiteralTrue
    | WLiteralFalse
    | WSelf
    | WStringLiteral String
    | WVariable Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WCatch = WCatch Ident ExceptionType WBlockOrExpression
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WThenAlways
    = WThenAlwaysProvided WBlockOrExpression | WNoThenAlways
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ExceptionType
    = ProvidedExceptionType Ident | DefaultExceptionType
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpOr = OpOr1 | OpOr_or
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpAnd = OpAnd1 | OpAnd_and
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpEq = OpEq1 | OpEq2 | OpEq3 | OpEq4
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpCmp = OpCmp1 | OpCmp2 | OpCmp3 | OpCmp4
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpAdd = OpAdd1 | OpAdd2
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpMult = OpMult1 | OpMult2 | OpMult3
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpPower = OpPower1
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpUnary = OpUnary_not | OpUnary1 | OpUnary2 | OpUnary3
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data OpPostfix = OpPostfix1 | OpPostfix2
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WClosureParameters = WNoParameters | WWithParameters [Ident]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WElse = WNoElse | WElse WBlockOrExpression
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data WNewParameter = WNewParameter Ident WExpression
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

