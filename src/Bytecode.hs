module Bytecode where

import Data.Map.Strict (Map)

data WollokBytecode = WollokBytecode
  { programBytecode :: [Instruction]
  , classesBytecode :: CompiledClasses
  } deriving (Show, Eq)

type CompiledClasses = Map ClassName WollokCompiledClass

data WollokCompiledClass = WollokCompiledClass (Map String (Maybe [Instruction])) (Map Selector MethodImplementation)
  deriving (Show, Eq)

data MethodImplementation = Custom [String] [Instruction] | Native ClassName Selector
  deriving (Show, Eq)

data Selector = Selector String Int
  deriving (Show, Eq, Ord)

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

data Instruction
  = PushInteger Integer
  | PushBoolean Bool
  | PushNull
  | PushSelf
  | Send Selector
  | Return
  | CreateInstance ClassName [String]
  | SetInstanceVariable String
  | SetVariable String
  | PushVariable String
  | DeclareLocalVariable String
  | JumpIfFalse Int
  | Jump Int
  | PushClosure [Instruction]
  deriving (Show, Eq)
