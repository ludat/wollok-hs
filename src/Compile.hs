{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
  | PushSelf
  | Send Selector
  deriving (Show, Eq)

data RuntimeValue
  = WInteger Integer
  | WBoolean Bool
  deriving (Show, Eq)

data StackFrame = StackFrame
  { valueStack :: Stack RuntimeValue
  , self :: RuntimeValue
  }
  deriving (Show, Eq)

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
classOf (WBoolean _) = ClassName "Boolean"

toList :: Stack a -> [a]
toList stack =
  case stackPop stack of
    Nothing -> []
    (Just (stackWithoutElement, firstElement)) ->
      firstElement : toList stackWithoutElement

vmInitialState :: CompiledClasses -> VmState
vmInitialState compiledClasses =
  VmState
    { vmStack = emptyStackFrame
    , vmClassesBytecode = compiledClasses
    }

emptyStackFrame :: Stack StackFrame
emptyStackFrame = stackPush stackNew $ StackFrame { valueStack = stackNew, self = error "no self" }

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
nameFrom (WAddOpSelector OpAdd1) = "+"
nameFrom (WAddOpSelector OpAdd2) = "-"
nameFrom (WOrOpSelector OpOr1) = "||"
nameFrom (WOrOpSelector OpOr_or) = "or"
nameFrom (WAndOpSelector OpAnd1) = "&&"
nameFrom (WAndOpSelector OpAnd_and) = "and"
nameFrom (WMultOpSelector OpMult1) = "*"
nameFrom (WMultOpSelector OpMult2) = "/"
nameFrom (WMultOpSelector OpMult3) = "%"
nameFrom (WEqOpSelector OpEq1) = "=="
nameFrom (WEqOpSelector OpEq2) = "!="
nameFrom (WEqOpSelector OpEq3) = "==="
nameFrom (WEqOpSelector OpEq4) = "!=="
nameFrom (WCmpOpSelector OpCmp1) = ">="
nameFrom (WCmpOpSelector OpCmp2) = "<="
nameFrom (WCmpOpSelector OpCmp3) = ">"
nameFrom (WCmpOpSelector OpCmp4) = "<"
nameFrom (WPowerOpSelector OpPower1) = "**"
nameFrom (WUnaryOpSelector OpUnary_not) = "not"
nameFrom (WUnaryOpSelector OpUnary1) = "!"
nameFrom (WUnaryOpSelector OpUnary2) = "-"
nameFrom (WUnaryOpSelector OpUnary3) = "+"
nameFrom (WPostfixOpSelector OpPostfix1) = "++"
nameFrom (WPostfixOpSelector OpPostfix2) = "--"

compileMethodBody :: ClassName -> Selector -> MethodBody -> MethodImplementation
compileMethodBody className selector methodBody =
  case methodBody of
    ImplementedNatively -> Native className selector
    (ImplementedByExpression e) -> Custom $ compileExpression e
    (ImplementedByBlock l_w) -> undefined

compileExpression :: WExpression -> [Instruction]
compileExpression (WNumberLiteral i) = [Push $ WInteger i]
compileExpression (WAddExpression receiver opAdd argument) =
  compileExpression $ WMessageSend receiver (Ident $ nameFrom (WAddOpSelector opAdd)) [argument]
compileExpression (WMessageSend receiver (Ident messageName) arguments) =
  let numberOfArguments = length arguments
  in
    compileExpression receiver ++
    concatMap compileExpression arguments ++
    [ Send $ Selector messageName numberOfArguments ]
compileExpression WSelf = [ PushSelf ]
compileExpression x = error $ show x

compileStatement :: WStatement -> [Instruction]
compileStatement (TopLevelExpression e) = compileExpression e
compileStatement (VarDeclaration w1) = undefined
compileStatement (WReturn w1) = undefined
compileStatement (WThrow w1) = undefined
compileStatement (WAssignment i w2) = undefined

run :: WollokBytecode -> VmState
run (WollokBytecode {..}) =
  snd $ State.runState (forM programBytecode runInstruction) (vmInitialState classesBytecode)

runInstruction :: Instruction -> ExecutionM ()
runInstruction (Push value) = push value

runInstruction PushSelf = do
  vmStack <- State.gets vmStack
  let Just StackFrame {..} = stackPeek vmStack
  push self

runInstruction (Send selector) = do
  let (Selector _ numberOfArguments) = selector
  arguments <- popMany numberOfArguments
  receiver <- pop
  vmState <- State.get
  let wollokClass = lookupClassOf vmState receiver
  let Just wollokMethod = lookupMethod wollokClass selector
  pushStackFrame receiver
  case wollokMethod of
    Custom instructions -> do
      forM_ instructions runInstruction
    Native className' selector' -> do
      runNativeMethod receiver arguments className' selector'

pushStackFrame :: RuntimeValue -> ExecutionM ()
pushStackFrame receiver = do
  vmState <- State.get
  State.put $ vmState { vmStack = stackPush (vmStack vmState) (StackFrame stackNew receiver) }

runNativeMethod :: RuntimeValue -> [RuntimeValue] -> ClassName -> Selector -> ExecutionM ()
runNativeMethod receiver arguments className selector =
  case (className, selector) of
    (ClassName "Number", Selector "+" 1) -> do
      let WInteger n1 = receiver
      let [WInteger n2] = arguments
      push $ WInteger (n1 + n2)
    (ClassName "Number", Selector "-" 1) -> do
      let WInteger n1 = receiver
      let [WInteger n2] = arguments
      push $ WInteger (n1 - n2)
    (ClassName "Number", Selector "between" 2) -> do
      let WInteger self = receiver
      let [WInteger minN, WInteger maxN] = arguments
      push $ WBoolean $ (self >= minN) && (self <= maxN)
    lookedUpMethod -> error $ "Couldn't find method: " ++ show lookedUpMethod

lookupClassOf :: VmState -> RuntimeValue -> WollokCompiledClass
lookupClassOf (VmState {..}) object =
  let className = classOf object in
    case Map.lookup className vmClassesBytecode of
      Just c -> c
      Nothing -> error $ "Class not found: " ++ show className

lookupMethod :: WollokCompiledClass -> Selector -> Maybe MethodImplementation
lookupMethod (WollokCompiledClass methodDictionary) selector =
  Map.lookup selector methodDictionary

popMany :: Int -> ExecutionM [RuntimeValue]
popMany n = fmap reverse $ replicateM n pop

pop :: ExecutionM RuntimeValue
pop = do
  vmState <- State.get
  let Just (restOfFrames, StackFrame valuesStack self) = stackPop (vmStack vmState)
  let Just (restOfValues, value) = stackPop valuesStack
  State.put $ vmState { vmStack = stackPush restOfFrames (StackFrame restOfValues self) }
  pure value

push :: RuntimeValue -> ExecutionM ()
push value = do
  vmState <- State.get
  let Just (restOfFrames, StackFrame valuesStack self) = stackPop (vmStack vmState)
  State.put $ vmState { vmStack = stackPush restOfFrames (StackFrame (stackPush valuesStack value) self) }
