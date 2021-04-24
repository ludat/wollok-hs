module Compile where

import AST

data WollokBytecode
  = Noop
  deriving (Show, Eq)

compile :: WollokAST -> [WollokBytecode]
compile (Package {..}) = undefined
compile (Class {..}) = undefined
compile (Method {..}) = undefined
compile (Body {..}) = undefined
compile (Return {..}) = undefined
compile (LiteralNumber {..}) = undefined
compile (LiteralString {..}) = undefined
compile LiteralNull = undefined
compile (Literal {..}) = undefined
compile (Reference {..}) = undefined
compile (Variable {..}) = undefined
compile (Assignment {..}) = undefined
compile (New {..}) = undefined
compile (Send {..}) = undefined
compile (Field {..}) = undefined
compile (Singleton {..}) = undefined
compile (Program {..}) = undefined