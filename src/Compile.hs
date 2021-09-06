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
  | SetVariable String
  | PushVariable String
  | DeclareLocalVariable String
  | JumpIfFalse Int
  | Jump Int
  | PushClosure [Instruction]
  | Throw
  deriving (Show, Eq)

data RuntimeValue
  = WInteger Integer
  | WBoolean Bool
  | WNull
  | WObjectReference ObjectId
  | WClosure ObjectId RuntimeValue [Instruction]
  deriving (Show, Eq)

type ObjectId = Int

data WObject
  = WObject ClassName (Map String RuntimeValue)
  | WContext (Maybe ObjectId) RuntimeValue (Map String RuntimeValue)
  deriving (Show, Eq)

data StackFrame = StackFrame
  { valueStack :: Stack RuntimeValue
  , thisContext :: ObjectId
  , programCounter :: Int
  , instructions :: [Instruction]
  } deriving (Show)

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
classOf (WClosure _ _ _) = pure $ ClassName "Closure"
classOf (WObjectReference objectId) = do
  dereference objectId
    >>= \case
      WObject className _ ->
        pure className
      WContext _ _ _ -> pure $ ClassName "Context"

toList :: Stack a -> [a]
toList stack =
  case stackPop stack of
    Nothing -> []
    (Just (stackWithoutElement, firstElement)) ->
      firstElement : toList stackWithoutElement

vmInitialState :: WollokBytecode -> VmState
vmInitialState (WollokBytecode {..}) =
  let
    self = WNull
    initialContext = WContext Nothing self Map.empty
    initialContextId = 1
    emptyStackFrame =
      stackPush stackNew $
        StackFrame
          { valueStack = stackNew
          , thisContext = initialContextId
          , programCounter = 0
          , instructions = programBytecode
          }
  in
  VmState
    { vmStack = emptyStackFrame
    , vmClassesBytecode = classesBytecode
    , vmObjectSpace = Map.singleton initialContextId initialContext
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
compileExpression (WTry block catchBlocks WNoThenAlways) =
  compileStatements $ toStatementList block

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
compileStatement (WThrow e) = compileExpression e ++ [ Throw ]
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
  (_, self, _) <- getThisContext
  pure self

getThisContext :: ExecutionM (Maybe ObjectId, RuntimeValue, Map String RuntimeValue)
getThisContext = do
  stackFrame <- currentStackFrame
  dereference (thisContext stackFrame) >>= \case
    WContext parentContext self localVariables -> pure (parentContext, self, localVariables)
    _ -> undefined

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
          (\(name, instructions) -> instructions ++ [SetVariable name])
          instanceVariablesInstructions
        ++ [PushSelf, Return]

  newObjectId <- allocateObject newObject

  pushNewStackFrame Nothing (WObjectReference newObjectId) Map.empty initializeInstructions

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

runInstruction (SetVariable variableName) = do
  newValue <- pop
  contextId <- lookupContextWithVariable variableName
  updateReference (fromJust contextId) $ \case
    (WContext parent self localVariables) ->
      WContext parent self $
      Map.insert variableName newValue localVariables
    (WObject className instanceVariables) ->
      WObject className $
      Map.insert variableName newValue instanceVariables

runInstruction (PushClosure instructions) = do
  stackFrame <- currentStackFrame
  self <- getSelf
  push $ WClosure (thisContext stackFrame) self instructions
runInstruction Throw = do
  undefined

lookupContextWithVariable :: String -> ExecutionM (Maybe ObjectId)
lookupContextWithVariable variableName = do
  stackFrame <- currentStackFrame
  go (thisContext stackFrame)
  where
    go contextId =
      dereference contextId >>= \case
        (WContext parentContextId self localVariables) -> do
          case Map.lookup variableName localVariables of
            Just _ -> pure $ Just contextId
            Nothing -> do
              case parentContextId of
                Just id' -> do
                  go id'
                Nothing ->
                  case self of
                    WObjectReference objectId -> pure $ Just objectId -- TODO: tendríamos que fijarnos si el objeto posta tiene o no definida la variable
                    _ -> pure $ Nothing
        _ -> undefined

allocateObject :: WObject -> ExecutionM ObjectId
allocateObject newObject = do
  vmState <- State.get
  let newId = (1+) $ maximum $ (0 :) $ Map.keys $ vmObjectSpace vmState
      newObjectSpace = Map.insert newId newObject $ vmObjectSpace vmState
  State.put $ vmState { vmObjectSpace = newObjectSpace }
  pure newId

lookupVariable :: String -> ExecutionM RuntimeValue
lookupVariable variableName = do
  contextId <- lookupContextWithVariable variableName
  dereference (fromJust contextId) >>= \case
    (WContext _ _ localVariables) ->
      pure $ fromJust $ Map.lookup variableName localVariables
    (WObject _ instanceVariables) ->
      pure $ fromJust $ Map.lookup variableName instanceVariables

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
  stackFrame <- currentStackFrame
  updateReference (thisContext stackFrame) $
    \(WContext parent self localVariables) ->
        WContext parent self $
          Map.insert name value localVariables

setLocalVariable :: String -> RuntimeValue -> ExecutionM ()
setLocalVariable name value = do
  stackFrame <- currentStackFrame
  updateReference (thisContext stackFrame) $
    \(WContext parent self localVariables) ->
        WContext parent self $
          Map.insert name value localVariables

activateMethod :: RuntimeValue -> MethodImplementation -> [RuntimeValue] -> ExecutionM ()
activateMethod receiver method arguments = do
  case method of
    Native className' selector' -> do
      runNativeMethod receiver arguments className' selector'
    Custom parameters instructions -> do
      let localVariables = Map.fromList $ zip parameters arguments
      pushNewStackFrame Nothing receiver localVariables instructions

pushNewStackFrame :: Maybe ObjectId -> RuntimeValue -> Map String RuntimeValue -> [Instruction] -> ExecutionM ()
pushNewStackFrame parentContextId self localVariables instructions = do
  newContextId <- allocateObject $ WContext parentContextId self localVariables
  State.modify $ \vmState ->
    vmState
      { vmStack = stackPush
          (vmStack vmState)
          StackFrame
            { valueStack = stackNew
            , thisContext = newContextId
            , programCounter = 0
            , instructions = instructions
            }
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
      let WClosure parentContextId self instructions = receiver
      pushNewStackFrame (Just parentContextId) self Map.empty instructions
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
