{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compile where

import Parser.AbsGrammar
import Data.Stack
import Data.Function ( on )
import Control.Monad.State.Strict ( State )
import qualified Control.Monad.State.Strict as State
import Control.Monad

data WollokBytecode = WollokBytecode { programBytecode :: [Instruction] }
  deriving (Show, Eq)

data Instruction
  = Push RuntimeValue
  | Send String Int
  deriving (Show, Eq)

data RuntimeValue = WInteger Integer
  deriving (Show, Eq)

type StackFrame = RuntimeValue

data VmState = VmState { vmStack :: Stack StackFrame }
  deriving (Show)

type ExecutionM = State VmState

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
  WollokBytecode { programBytecode = concatMap compileStatement statements }

compileStatement :: WStatement -> [Instruction]
compileStatement (TopLevelExpression (WNumberLiteral n)) = [ Push $ WInteger n ]
compileStatement (TopLevelExpression (WAddExpression (WNumberLiteral n1) _ (WNumberLiteral n2))) =
  [
    Push $ WInteger n1,
    Push $ WInteger n2,
    Send "+" 1
  ]
compileStatement (TopLevelExpression (WAddExpression _ _ _)) = undefined
compileStatement (TopLevelExpression (WTry w l_w w4)) = undefined
compileStatement (TopLevelExpression (WOrExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WAndExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WEqExpression w o w4)) = undefined
compileStatement (TopLevelExpression (WCmpExpression w o w4)) = undefined
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
run (WollokBytecode instructions) vmState =
  snd $ State.runState (forM instructions runInstruction) vmState

runInstruction :: Instruction -> ExecutionM ()
runInstruction (Push value) = push value

runInstruction (Send selector numberOfArguments) = do
  WInteger n1 <- pop
  WInteger n2 <- pop
  push $ WInteger (n1 + n2)

pop :: ExecutionM StackFrame
pop = do
  vmState <- State.get
  let Just (restOfStack, value) = stackPop (vmStack vmState)
  State.put $ vmState { vmStack = restOfStack }
  pure value

push :: StackFrame -> ExecutionM ()
push stackFrame = do
  vmState <- State.get
  State.put $ vmState { vmStack = stackPush (vmStack vmState) stackFrame }
