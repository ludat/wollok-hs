{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Compile where

import Parser.AbsGrammar
import qualified Data.Map.Strict as Map

import Bytecode

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
          Custom parameters $ compileStatements statements ++ [PushNull, Return]
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
compileExpression (WNumberLiteral i) = [PushInteger i]
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
compileExpression (WLiteralTrue) = [ PushBoolean True ]
compileExpression (WLiteralFalse) = [ PushBoolean False ]
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
  = [ PushNull, DeclareLocalVariable variableName ]
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