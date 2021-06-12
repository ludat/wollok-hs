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
import Data.Maybe (fromJust)

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
  = Push RuntimeValue
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

data RuntimeValue
  = WInteger Integer
  | WBoolean Bool
  | WNull
  | WObjectReference ObjectId
  | WClosure RuntimeValue [Instruction]
  deriving (Show, Eq)

type ObjectId = Int

data WObject
  = WObject ClassName (Map String RuntimeValue)
  | WContext RuntimeValue (Map String RuntimeValue)
  deriving (Show, Eq)

data StackFrame = StackFrame
  { valueStack :: Stack RuntimeValue
  , self :: RuntimeValue
  , localVariableBindings :: Map String RuntimeValue
  , programCounter :: Int
  , instructions :: [Instruction]
  }

instance Show StackFrame where
  show StackFrame {..} =
    unwords undefined

data VmState = VmState
  { vmStack :: Stack StackFrame
  , vmClassesBytecode :: CompiledClasses
  , vmObjectSpace :: Map ObjectId WObject
  } deriving (Show)

type ExecutionM = State VmState

instance Eq a => Eq (Stack a) where
  (==) = (==) `on` toList

classOf :: RuntimeValue -> ExecutionM ClassName
classOf (WInteger _) = pure $ ClassName "Number"
classOf (WBoolean _) = pure $ ClassName "Boolean"
classOf (WNull) = pure $ ClassName "Null"
classOf (WClosure _ _) = pure $ ClassName "Closure"
classOf (WObjectReference objectId) = do
  dereference objectId
    >>= \case
      WObject className _ ->
        pure className
      WContext _ _ -> pure $ ClassName "Context"

toList :: Stack a -> [a]
toList stack =
  case stackPop stack of
    Nothing -> []
    (Just (stackWithoutElement, firstElement)) ->
      firstElement : toList stackWithoutElement

vmInitialState :: WollokBytecode -> VmState
vmInitialState (WollokBytecode {..}) =
  let
    emptyStackFrame =
      stackPush stackNew $
        StackFrame
        { valueStack = stackNew
        , self = error "no self"
        , localVariableBindings = Map.empty
        , programCounter = 0
        , instructions = programBytecode
        }
  in
  VmState
    { vmStack = emptyStackFrame
    , vmClassesBytecode = classesBytecode
    , vmObjectSpace = Map.empty
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
  (WTopLevelClass (WClassDeclaration (Ident classIdentifier)
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
compileClass
  (WTopLevelObject _) = undefined

compileMethod :: ClassName -> WMethodDeclaration -> (Selector, MethodImplementation)
compileMethod className (WMethodDeclaration name parameterIdents body) =
  let
    parameters = fmap identToString parameterIdents
    selector = Selector (nameFrom name) (length parameters)
    compiledMethodBody =
      case body of
        ImplementedNatively ->
          Native className selector
        (ImplementedByExpression e) ->
          Custom parameters $ compileExpression e ++ [Return]
        (ImplementedByBlock statements) ->
          Custom parameters $ compileStatements statements ++ [Push WNull, Return]
  in (selector, compiledMethodBody)

identToString :: Ident -> String
identToString (Ident s) = s

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
compileExpression WSelf = [PushSelf]
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
compileExpression (WVariable (Ident variableName)) = [ PushVariable variableName ]
compileExpression (WIf condition t WNoElse) = undefined
compileExpression (WIf condition thenBlock (WElse elseBlock))
  = let
      compiledThen = compileStatements $ toStatementList thenBlock
      compiledElse = compileStatements $ toStatementList elseBlock
    in
      compileExpression condition ++ [ JumpIfFalse $ length compiledThen + 1 ]
        ++ compiledThen ++ [ Jump $ length compiledElse ]
        ++ compiledElse
compileExpression (WLiteralTrue) = [ Push $ WBoolean True ]
compileExpression (WLiteralFalse) = [ Push $ WBoolean False ]
compileExpression (WClosureLiteral WNoParameters body) =
  [ PushClosure $ compileStatements body ++ [Return] ]
compileExpression (WClosureLiteral (WWithParameters []) body) =
  [ PushClosure $ compileStatements body ++ [Return] ]

compileExpression x = error $ show x

compileStatements :: [WStatement] -> [Instruction]
compileStatements = concatMap compileStatement

toStatementList :: WBlockOrStatement -> [WStatement]
toStatementList (SingleExpression statement) = [statement]
toStatementList (Block statements) = statements

compileStatement :: WStatement -> [Instruction]
compileStatement (TopLevelExpression e) = compileExpression e
compileStatement
  (VarDeclaration (WVariableDeclaration Var
                                        (Ident variableName)
                                        (WithInitialValue initialValueExpression)))
  = compileExpression initialValueExpression ++ [ DeclareLocalVariable variableName ]
compileStatement
  (VarDeclaration (WVariableDeclaration Var
                                        (Ident variableName)
                                        NoIntialValue))
  = [ Push WNull, DeclareLocalVariable variableName ]
compileStatement
  (VarDeclaration (WVariableDeclaration Const
                                        (Ident variableName)
                                        (WithInitialValue initialValueExpression)))
    -- TODO: Eventually we'll need to remember that this was a const
  = compileExpression initialValueExpression ++ [ DeclareLocalVariable variableName ]
compileStatement
  (VarDeclaration (WVariableDeclaration Const i NoIntialValue))
  = undefined
compileStatement (WReturn e) = compileExpression e ++ [Return]
compileStatement (WThrow w1) = undefined
compileStatement (WAssignment (Ident variableName) expression)
  = compileExpression expression ++ [ SetVariable variableName ]

run :: WollokBytecode -> VmState
run wollokBytecode =
  snd $
    runIdentity $
    (`State.runStateT` vmInitialState wollokBytecode) $
    runInstructions

runInstructions :: ExecutionM ()
runInstructions = do
  maybeNextInstruction <- fetchNextInstruction
  case maybeNextInstruction of
    Just instruction -> do
      runInstruction instruction
      runInstructions
    Nothing -> pure ()

fetchNextInstruction :: ExecutionM (Maybe Instruction)
fetchNextInstruction = do
  StackFrame {..} <- currentStackFrame
  let currentInstruction = instructions !? programCounter
  incrementPc
  pure currentInstruction

infix 9 !?
(!?) :: [a] -> Int -> Maybe a
(!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing
{-# INLINE (!?) #-}

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
  activateMethod receiver wollokMethod arguments

runInstruction Return = do
  VmState {..} <- State.get
  valueToReturn <- pop
  stackFrame <- popStackFrame
  push valueToReturn

runInstruction (CreateInstance className constructorArgumentNames) = do
  (WollokCompiledClass instanceVariableDefinitions _) <- lookupClass className

  constructorArgumentValues <- popMany $ length constructorArgumentNames
  let constructorArguments = Map.fromList $ zip constructorArgumentNames (fmap (\value -> [Push value]) constructorArgumentValues)

  let instanceVariablesInstructions = Map.toList $
        Map.union constructorArguments $ Map.mapMaybe id instanceVariableDefinitions

  let newObject = WObject className Map.empty

  let initializeInstructions =
        concatMap
          (\(name, instructions) -> instructions ++ [SetInstanceVariable name])
          instanceVariablesInstructions
        ++ [PushSelf, Return]

  newObjectId <- allocateObject newObject

  pushNewStackFrame (WObjectReference newObjectId) Map.empty initializeInstructions

runInstruction (PushVariable variableName) = do
  variableValue <- lookupVariable variableName
  push variableValue

runInstruction (DeclareLocalVariable variableName) = do
  intialValue <- pop
  declareLocalVariable variableName intialValue

runInstruction (JumpIfFalse offset) = do
  conditionValue <- pop
  case conditionValue of
    WBoolean True -> do
      pure ()
    WBoolean False -> do
      offsetPcBy offset
    _ -> error "jump condition was not a boolean"

runInstruction (Jump offset) = do
  offsetPcBy offset

runInstruction (SetInstanceVariable variableName) = do
  newValue <- pop
  setInstanceVariable variableName newValue

runInstruction (SetVariable variableName) = do
  newValue <- pop
  stackFrame <- currentStackFrame
  case Map.lookup variableName $ localVariableBindings stackFrame of
    Just _ ->
      setLocalVariable variableName newValue
    Nothing ->
      setInstanceVariable variableName newValue

runInstruction (PushClosure instructions) = do
  self <- getSelf
  push $ WClosure self instructions

allocateObject :: WObject -> ExecutionM ObjectId
allocateObject newObject = do
  vmState <- State.get
  let newId = maximum $ (0 :) $ Map.keys $ vmObjectSpace vmState
      newObjectSpace = Map.insert newId newObject $ vmObjectSpace vmState
  State.put $ vmState { vmObjectSpace = newObjectSpace }
  pure newId

setInstanceVariable :: String -> RuntimeValue -> ExecutionM ()
setInstanceVariable variableName newValue = do
  self <- getSelf
  let WObjectReference objectId = self
  updateReference objectId $
    \(WObject className instanceVariables) ->
        WObject className $
          Map.insert variableName newValue instanceVariables

lookupVariable :: String -> ExecutionM RuntimeValue
lookupVariable variableName = do
  maybeLocalVariable <- lookupLocalVariable variableName
  case maybeLocalVariable of
    Just localVariableValue -> do
      pure localVariableValue
    Nothing -> do
      self <- getSelf
      let WObjectReference objectId = self
      dereference objectId
        >>= \case
          WObject _ instanceVariables ->
            pure $ fromJust $ Map.lookup variableName instanceVariables
          _ -> error $ "encontre un Context en self mientras buscaba la variable: '" ++ variableName ++ "'"

updateReference :: ObjectId -> (WObject -> WObject) -> ExecutionM ()
updateReference objectId f = do
  vmState <- State.get
  let newObjectSpace = Map.update (Just . f) objectId $ vmObjectSpace vmState
  State.put $ vmState { vmObjectSpace = newObjectSpace }

dereference :: ObjectId -> ExecutionM WObject
dereference objectId = do
  vmState <- State.get
  pure $ fromJust $ Map.lookup objectId $ vmObjectSpace vmState

declareLocalVariable :: String -> RuntimeValue -> ExecutionM ()
declareLocalVariable name value = do
  modifyStackFrame $
    \stackFrame ->
       (stackFrame { localVariableBindings = Map.insert name value $ localVariableBindings stackFrame }, ())

setLocalVariable :: String -> RuntimeValue -> ExecutionM ()
setLocalVariable name value = do
  modifyStackFrame $
    \stackFrame ->
       (stackFrame { localVariableBindings = Map.insert name value $ localVariableBindings stackFrame }, ())

lookupLocalVariable :: String -> ExecutionM (Maybe RuntimeValue)
lookupLocalVariable name = do
  vmState <- State.get
  let Just stackFrame = stackPeek (vmStack vmState)
  pure $ Map.lookup name $ localVariableBindings stackFrame

activateMethod :: RuntimeValue -> MethodImplementation -> [RuntimeValue] -> ExecutionM ()
activateMethod receiver method arguments = do
  case method of
    Native className' selector' -> do
      runNativeMethod receiver arguments className' selector'
    Custom parameters instructions -> do
      let localVariables = Map.fromList $ zip parameters arguments
      pushNewStackFrame receiver localVariables instructions

pushNewStackFrame :: RuntimeValue -> Map String RuntimeValue -> [Instruction] -> ExecutionM ()
pushNewStackFrame self localVariables instructions = do
  vmStateInicial <- State.get
  State.put $ vmStateInicial
    { vmStack = stackPush
        (vmStack vmStateInicial)
        (StackFrame stackNew self localVariables 0 instructions)
    }

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
    (ClassName "Closure", Selector "apply" 0) -> do
      let WClosure self instructions = receiver
      pushNewStackFrame self Map.empty instructions
    lookedUpMethod -> error $ "Couldn't find method: " ++ show lookedUpMethod

lookupClassOf :: RuntimeValue -> ExecutionM WollokCompiledClass
lookupClassOf object = do
  lookupClass =<< classOf object

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

incrementPc :: ExecutionM ()
incrementPc = offsetPcBy 1

offsetPcBy :: Int -> ExecutionM ()
offsetPcBy offset = modifyPc (+ offset)

modifyPc :: (Int -> Int) -> ExecutionM ()
modifyPc f = do
  modifyStackFrame $ \stackFrame -> (stackFrame { programCounter = f $ programCounter stackFrame}, ())

popStackFrame :: ExecutionM StackFrame
popStackFrame = do
  vmState <- State.get
  let Just (restOfFrames, stackFrame) = stackPop (vmStack vmState)
  State.put $ vmState { vmStack = restOfFrames }
  pure stackFrame

modifyStackFrame :: (StackFrame -> (StackFrame, a)) -> ExecutionM a
modifyStackFrame modifyFunction = do
  vmState <- State.get
  let Just (restOfFrames, stackFrame) = stackPop (vmStack vmState)
  let (updatedStackFrame, result) = modifyFunction stackFrame
  State.put $ vmState
    { vmStack = stackPush restOfFrames updatedStackFrame
    }
  pure result

pop :: ExecutionM RuntimeValue
pop = do
  modifyValueStack $ \valuesStack -> fromJust $ stackPop valuesStack

push :: RuntimeValue -> ExecutionM ()
push value = do
  modifyValueStack $ \valuesStack -> (stackPush valuesStack value, ())

modifyValueStack :: (Stack RuntimeValue -> (Stack RuntimeValue, a)) -> ExecutionM a
modifyValueStack modifyFunction = do
  modifyStackFrame $ \stackFrame ->
    let
      (updatedValues, result) = modifyFunction (valueStack stackFrame)
    in
      (stackFrame {valueStack = updatedValues}, result)
