{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compile where

import Parser.AbsGrammar
import Data.Stack
import Data.Function ( on )
import Control.Monad.State.Strict
import qualified Control.Monad.State.Strict as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Identity
import Control.Monad.Cont
import Data.List (intercalate)
import Data.Maybe (fromJust)

data WollokBytecode = WollokBytecode
  { programBytecode :: [Instruction]
  , classesBytecode :: CompiledClasses
  } deriving (Show, Eq)

type CompiledClasses = Map ClassName WollokCompiledClass

data WollokCompiledClass = WollokCompiledClass (Map String (Maybe [Instruction])) (Map Selector MethodImplementation)
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
  | Return
  | CreateInstance ClassName [String]
  | PushInstanceVariable String
  deriving (Show, Eq)

data RuntimeValue
  = WInteger Integer
  | WBoolean Bool
  | WNull
  | WObject ClassName (Map String RuntimeValue)
  deriving (Show, Eq)

data StackFrame = StackFrame
  { valueStack :: Stack RuntimeValue
  , self :: RuntimeValue
  , returnFunction :: () -> ExecutionM ()
  }

instance Show StackFrame where
  show (StackFrame {..}) =
    intercalate " " undefined

data VmState = VmState
  { vmStack :: Stack StackFrame
  , vmClassesBytecode :: CompiledClasses
  } deriving (Show)

type ExecutionM = ContT () (State VmState)

instance Eq a => Eq (Stack a) where
  (==) = (==) `on` toList

classOf :: RuntimeValue -> ClassName
classOf (WInteger _) = ClassName "Number"
classOf (WBoolean _) = ClassName "Boolean"
classOf (WNull) = ClassName "Null"
classOf (WObject className _) = className

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
emptyStackFrame =
  stackPush stackNew $
    StackFrame
    { valueStack = stackNew
    , self = error "no self"
    , returnFunction = (error "no return in empty stackframe")
    }

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
  = let
      className = ClassName classIdentifier
      compiledMethods = Map.fromList $ map (compileMethod className) methods
      compiledInstanceVariables = Map.fromList $ map compileInstanceVariable instanceVariables
    in
      (className, WollokCompiledClass compiledInstanceVariables compiledMethods)
    where
      compileInstanceVariable :: WVariableDeclaration -> (String, Maybe [Instruction])
      compileInstanceVariable
        (WVariableDeclaration _ (Ident variableName) (WithInitialValue variableValueExpression))
        = (variableName, Just $ compileExpression variableValueExpression)
      compileInstanceVariable
        (WVariableDeclaration _ (Ident variableName) NoIntialValue)
        = (variableName, Nothing)

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
    (ImplementedByExpression e) -> Custom $ compileExpression e ++ [Return]
    (ImplementedByBlock statements) -> Custom $ concatMap compileStatement statements

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
compileExpression (WNew (Ident classIdentifier) arguments) =
  let
    compiledArguments =
      concatMap (compileExpression . extractParameterExpression) arguments
    compiledConstructorCall =
      [ CreateInstance (ClassName classIdentifier) (fmap extractParameterIdent arguments) ]
  in
    compiledArguments ++ compiledConstructorCall
  where
    extractParameterIdent (WNewParameter (Ident argumentName) _) = argumentName
    extractParameterExpression (WNewParameter _ expression) = expression
compileExpression (WVariable (Ident variableName)) = [ PushInstanceVariable variableName ]

compileExpression x = error $ show x

compileStatement :: WStatement -> [Instruction]
compileStatement (TopLevelExpression e) = compileExpression e
compileStatement (VarDeclaration w1) = undefined
compileStatement (WReturn e) = compileExpression e ++ [Return]
compileStatement (WThrow w1) = undefined
compileStatement (WAssignment i w2) = undefined

run :: WollokBytecode -> VmState
run (WollokBytecode {..}) =
  snd $
    runIdentity $
    (`State.runStateT` vmInitialState classesBytecode) $
    (`runContT` pure) $
    runInstructions programBytecode

runInstructions :: [Instruction] -> ExecutionM ()
runInstructions = mapM_ runInstruction

getSelf :: ExecutionM RuntimeValue
getSelf = do
  vmStack <- State.gets vmStack
  let Just StackFrame {..} = stackPeek vmStack
  pure self

runInstruction :: Instruction -> ExecutionM ()
runInstruction (Push value) =
  push value

runInstruction PushSelf = do
  push =<< getSelf

runInstruction (Send selector) = do
  let (Selector _ numberOfArguments) = selector
  arguments <- popMany numberOfArguments
  receiver <- pop
  wollokClass <- lookupClassOf receiver
  let Just wollokMethod = lookupMethod wollokClass selector
  withNewStackFrame receiver $ do
    case wollokMethod of
      Custom instructions -> do
        forM_ instructions runInstruction
        push WNull
      Native className' selector' -> do
        runNativeMethod receiver arguments className' selector'

runInstruction Return = do
  VmState {..} <- State.get
  stackFrame <- currentStackFrame
  returnFunction stackFrame ()

runInstruction (CreateInstance className constructorArgumentNames) = do
  (WollokCompiledClass instanceVariableDefinitions _) <- lookupClass className

  constructorArgumentValues <- popMany $ length constructorArgumentNames
  let constructorArguments = Map.fromList $ zip constructorArgumentNames (fmap (\value -> [Push value]) constructorArgumentValues)

  let instanceVariablesInstructions =
        Map.union constructorArguments $ Map.mapMaybe id instanceVariableDefinitions

  instanceVariables <- forM instanceVariablesInstructions $ \is -> do
    runInstructions is
    pop
  push $ WObject className instanceVariables

runInstruction (PushInstanceVariable variableName) = do
  self <- getSelf
  let WObject _ instanceVariables = self
  push $ fromJust $ Map.lookup variableName instanceVariables

withNewStackFrame :: RuntimeValue -> ExecutionM () -> ExecutionM ()
withNewStackFrame receiver actions = do
  vmStateInicial <- State.get
  callCC $ \exit -> do
    State.put $ vmStateInicial
      { vmStack = stackPush (vmStack vmStateInicial) (StackFrame stackNew receiver exit)
      }
    actions
  returnValue <- pop
  State.put vmStateInicial
  push returnValue

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

lookupClassOf :: RuntimeValue -> ExecutionM WollokCompiledClass
lookupClassOf object = do
  lookupClass $ classOf object

lookupClass :: ClassName -> ExecutionM WollokCompiledClass
lookupClass className = do
  (VmState {..}) <- State.get
  case Map.lookup className vmClassesBytecode of
    Just c -> pure c
    Nothing -> error $ "Class not found: " ++ show className

lookupMethod :: WollokCompiledClass -> Selector -> Maybe MethodImplementation
lookupMethod (WollokCompiledClass _ methodDictionary) selector =
  Map.lookup selector methodDictionary

popMany :: Int -> ExecutionM [RuntimeValue]
popMany n = fmap reverse $ replicateM n pop

currentStackFrame :: ExecutionM StackFrame
currentStackFrame = do
  vmState <- State.get
  let Just stackFrame = stackPeek (vmStack vmState)
  pure stackFrame

pop :: ExecutionM RuntimeValue
pop = do
  vmState <- State.get
  let Just (restOfFrames, StackFrame valuesStack self returnFunction) = stackPop (vmStack vmState)
  let Just (restOfValues, value) = stackPop valuesStack
  State.put $ vmState { vmStack = stackPush restOfFrames (StackFrame restOfValues self returnFunction) }
  pure value

push :: RuntimeValue -> ExecutionM ()
push value = do
  vmState <- State.get
  let Just (restOfFrames, StackFrame valuesStack self returnFunction) = stackPop (vmStack vmState)
  State.put $ vmState
    { vmStack = stackPush restOfFrames (StackFrame (stackPush valuesStack value) self returnFunction)
    }
