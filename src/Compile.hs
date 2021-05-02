{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
module Compile where

import Parser.AbsGrammar
import Data.Stack
import Data.Function ( on )
import Control.Monad.State.Strict ( State )
import qualified Control.Monad.State.Strict as State
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data WollokBytecode = WollokBytecode
  { programBytecode :: [Instruction]
  , classesBytecode :: CompiledClasses
  } deriving (Show, Eq)

type CompiledClasses = Map ClassName WollokCompiledClass

newtype WollokCompiledClass = WollokCompiledClass (Map Selector MethodImplementation)
  deriving (Show, Eq)

data MethodImplementation = Custom [Instruction] | Native ClassName Selector
  deriving (Show, Eq)

data Selector = Selector String Int
  deriving (Show, Eq, Ord)

newtype ClassName = ClassName String
  deriving (Show, Eq, Ord)

data Instruction
  = Push RuntimeValue
  | Send Selector
  deriving (Show, Eq)

data RuntimeValue = WInteger Integer
  deriving (Show, Eq)

type StackFrame = RuntimeValue

data VmState = VmState
  { vmStack :: Stack StackFrame
  , vmClassesBytecode :: CompiledClasses
  }
  deriving (Show, Eq)

type ExecutionM = State VmState

instance Eq a => Eq (Stack a) where
  (==) = (==) `on` toList

classOf :: RuntimeValue -> ClassName
classOf (WInteger _) = ClassName "Number"

toList :: Stack a -> [a]
toList stack =
  case stackPop stack of
    Nothing -> []
    (Just (stackWithoutElement, firstElement)) ->
      firstElement : toList stackWithoutElement

vmInitialState :: CompiledClasses -> VmState
vmInitialState compiledClasses = VmState { vmStack = stackNew, vmClassesBytecode = compiledClasses }

compile :: WFile -> WollokBytecode
compile (WFile imports classes program) =
  WollokBytecode
    { programBytecode = compileProgram program
    , classesBytecode = compileClasses classes
    }

compileProgram :: WProgram -> [Instruction]
compileProgram (WProgram _ statements) = concatMap compileStatement statements

compileClasses :: [WLibraryElement] -> CompiledClasses
compileClasses libraryElements = Map.fromList $ map compileClass libraryElements

compileClass :: WLibraryElement -> (ClassName, WollokCompiledClass)
compileClass
  (WLibraryElement (WClassDeclaration (Ident classIdentifier)
                                      superclassName
                                      instanceVariables
                                      methods))
  = (className, WollokCompiledClass $ Map.fromList $ map (compileMethod className) methods)
  where className = ClassName classIdentifier

compileMethod :: ClassName -> WMethodDeclaration -> (Selector, MethodImplementation)
compileMethod className (WMethodDeclaration name parameters body) =
  (selector, compileMethodBody className selector body)
  where selector = Selector (nameFrom name) (length parameters)

nameFrom :: WSelector -> String
nameFrom (WSelector (Ident name)) = name
nameFrom (WOpSelector OpAdd1) = "+"
nameFrom (WOpSelector OpAdd2) = "-"

-- TODO:
pattern SelectorWithName :: String -> WSelector
pattern SelectorWithName name = WSelector (Ident name)

compileMethodBody :: ClassName -> Selector -> MethodBody -> MethodImplementation
compileMethodBody className selector methodBody =
  case methodBody of
    ImplementedNatively -> Native className selector
    (ImplementedByBlock l_w) -> undefined
    (ImplementedByExpression w) -> undefined

compileStatement :: WStatement -> [Instruction]
compileStatement (TopLevelExpression (WNumberLiteral n)) = [ Push $ WInteger n ]
compileStatement (TopLevelExpression (WAddExpression (WNumberLiteral n1) _ (WNumberLiteral n2))) =
  [
    Push $ WInteger n2,
    Push $ WInteger n1,
    Send $ Selector "+" 1
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

run :: WollokBytecode -> VmState
run (WollokBytecode {..}) =
  snd $ State.runState (forM programBytecode runInstruction) (vmInitialState classesBytecode)

runInstruction :: Instruction -> ExecutionM ()
runInstruction (Push value) = push value

runInstruction (Send selector) = do
  receiver <- pop
  vmState <- State.get
  let wollokClass = lookupClassOf vmState receiver
  let Just wollokMethod = lookupMethod wollokClass selector
  case wollokMethod of
    Custom _ -> undefined
    Native className' selector' -> do
      let WInteger n1 = receiver
      WInteger n2 <- pop
      push $ WInteger (n1 + n2)

lookupClassOf :: VmState -> RuntimeValue -> WollokCompiledClass
lookupClassOf (VmState {..}) object =
  let className = classOf object in
    case Map.lookup className vmClassesBytecode of
      Just c -> c
      Nothing -> error $ "Class not found: " ++ show className

lookupMethod :: WollokCompiledClass -> Selector -> Maybe MethodImplementation
lookupMethod (WollokCompiledClass methodDictionary) selector =
  Map.lookup selector methodDictionary

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
