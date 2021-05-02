{-# LANGUAGE StandaloneDeriving #-}
module Compile where

import Parser.AbsGrammar
import Data.Stack
import Data.Function ( on )

data WollokBytecode = WollokBytecode { programBytecode :: [Instruction] }
  deriving (Show, Eq)
data Instruction = Push RuntimeValue
  deriving (Show, Eq)
data RuntimeValue = WInteger Integer
  deriving (Show, Eq)
data StackFrame = Value RuntimeValue
  deriving (Show, Eq)
data VmState = VmState { vmStack :: Stack StackFrame }
  deriving (Show)

instance Eq a => Eq (Stack a) where
  (==) = (==) `on` toList

toList :: Stack a -> [a]
toList stack =
  case stackPop stack of
    Nothing -> []
    (Just (stackWithoutElement, firstElement)) ->
      firstElement : toList stackWithoutElement

estadoInicialVm :: VmState
estadoInicialVm = VmState { vmStack = stackNew }

compile :: WFile -> WollokBytecode
compile (WFile imports classes program) = compileProgram program

compileProgram :: WProgram -> WollokBytecode
compileProgram (WProgram _ statements) =
  WollokBytecode { programBytecode = map compileStatement statements }

compileStatement :: WStatement -> Instruction
compileStatement (TopLevelExpression (WNumberLiteral n)) = Push $ WInteger n
compileStatement (TopLevelExpression (WTry w l_w w4)) = undefined
compileStatement (TopLevelExpression (WOrExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WAndExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WEqExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WCmpExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WAddExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WMultExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WPowerExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WUnaryExpression o w)) = undefined
compileStatement (TopLevelExpression (WPostfixExpression w o)) = undefined
compileStatement (TopLevelExpression (WMessageSend w i l_w)) = undefined
compileStatement (TopLevelExpression (WClosure w l_w)) = undefined
compileStatement (TopLevelExpression (WIf w w3 w4)) = undefined
compileStatement (TopLevelExpression (WObjectLiteral i w l_w l_w5))
  = undefined
compileStatement (TopLevelExpression WNullLiteral) = undefined
compileStatement (TopLevelExpression WLiteralTrue) = undefined
compileStatement (TopLevelExpression WLiteralFalse) = undefined
compileStatement (TopLevelExpression WSelf) = undefined
compileStatement (TopLevelExpression (WStringLiteral l_c)) = undefined
compileStatement (TopLevelExpression (WVariable i)) = undefined
compileStatement (VarDeclaration w1) = undefined
compileStatement (WReturn w1) = undefined
compileStatement (WThrow w1) = undefined
compileStatement (WAssignment i w2) = undefined

run :: WollokBytecode -> VmState -> VmState
run (WollokBytecode instructions) vmState = foldl runInstruction vmState instructions

runInstruction :: VmState -> Instruction -> VmState
runInstruction vmState (Push value) =
  vmState { vmStack = stackPush (vmStack vmState) (Value value) }
