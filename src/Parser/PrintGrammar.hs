{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for Parser.
--   Generated by the BNF converter.

module Parser.PrintGrammar where

import qualified Parser.AbsGrammar
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Parser.AbsGrammar.Ident where
  prt _ (Parser.AbsGrammar.Ident i) = doc $ showString $ i
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Parser.AbsGrammar.WFile where
  prt i e = case e of
    Parser.AbsGrammar.WFile imports wlibraryelements wprogram -> prPrec i 0 (concatD [prt 0 imports, prt 0 wlibraryelements, prt 0 wprogram])

instance Print [Parser.AbsGrammar.Import] where
  prt = prtList

instance Print [Parser.AbsGrammar.WLibraryElement] where
  prt = prtList

instance Print Parser.AbsGrammar.Import where
  prt i e = case e of
    Parser.AbsGrammar.Import -> prPrec i 0 (concatD [doc (showString "import"), doc (showString "-"), doc (showString "TODO")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.AbsGrammar.WLibraryElement where
  prt i e = case e of
    Parser.AbsGrammar.WLibraryElement wclassdeclaration -> prPrec i 0 (concatD [prt 0 wclassdeclaration])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.AbsGrammar.WProgram where
  prt i e = case e of
    Parser.AbsGrammar.WProgram id wstatements -> prPrec i 0 (concatD [doc (showString "program"), prt 0 id, doc (showString "{"), prt 0 wstatements, doc (showString "}")])

instance Print [Parser.AbsGrammar.WStatement] where
  prt = prtList

instance Print Parser.AbsGrammar.WClassDeclaration where
  prt i e = case e of
    Parser.AbsGrammar.WClassDeclaration id wsuperclassdeclaration wvariabledeclarations wmethoddeclarations -> prPrec i 0 (concatD [doc (showString "class"), prt 0 id, prt 0 wsuperclassdeclaration, doc (showString "{"), prt 0 wvariabledeclarations, prt 0 wmethoddeclarations, doc (showString "}")])

instance Print [Parser.AbsGrammar.WVariableDeclaration] where
  prt = prtList

instance Print [Parser.AbsGrammar.WMethodDeclaration] where
  prt = prtList

instance Print Parser.AbsGrammar.WSuperclassDeclaration where
  prt i e = case e of
    Parser.AbsGrammar.WSuperclass id -> prPrec i 0 (concatD [doc (showString "inherits"), prt 0 id])
    Parser.AbsGrammar.WNoSuperclass -> prPrec i 0 (concatD [])

instance Print Parser.AbsGrammar.WMethodDeclaration where
  prt i e = case e of
    Parser.AbsGrammar.WMethodDeclaration id ids methodbody -> prPrec i 0 (concatD [doc (showString "method"), prt 0 id, doc (showString "("), prt 0 ids, doc (showString ")"), prt 0 methodbody])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Parser.AbsGrammar.Ident] where
  prt = prtList

instance Print Parser.AbsGrammar.MethodBody where
  prt i e = case e of
    Parser.AbsGrammar.ImplementedByBlock wstatements -> prPrec i 0 (concatD [doc (showString "{"), prt 0 wstatements, doc (showString "}")])
    Parser.AbsGrammar.ImplementedByExpression wexpression -> prPrec i 0 (concatD [doc (showString "="), prt 0 wexpression])
    Parser.AbsGrammar.ImplementedNatively -> prPrec i 0 (concatD [doc (showString "native")])

instance Print Parser.AbsGrammar.WStatement where
  prt i e = case e of
    Parser.AbsGrammar.TopLevelExpression wexpression -> prPrec i 0 (concatD [prt 0 wexpression])
    Parser.AbsGrammar.VarDeclaration wvariabledeclaration -> prPrec i 0 (concatD [prt 0 wvariabledeclaration])
    Parser.AbsGrammar.WReturn wexpression -> prPrec i 0 (concatD [doc (showString "return"), prt 0 wexpression])
    Parser.AbsGrammar.WThrow wexpression -> prPrec i 0 (concatD [doc (showString "throw"), prt 0 wexpression])
    Parser.AbsGrammar.WAssignment id wexpression -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 wexpression])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.AbsGrammar.WVariableDeclaration where
  prt i e = case e of
    Parser.AbsGrammar.WVariableDeclaration wvariabletype id wexpression -> prPrec i 0 (concatD [prt 0 wvariabletype, prt 0 id, doc (showString "="), prt 0 wexpression])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.AbsGrammar.WVariableType where
  prt i e = case e of
    Parser.AbsGrammar.Var -> prPrec i 0 (concatD [doc (showString "var")])
    Parser.AbsGrammar.Const -> prPrec i 0 (concatD [doc (showString "const")])

instance Print Parser.AbsGrammar.WBlockOrExpression where
  prt i e = case e of
    Parser.AbsGrammar.SingleExpression wstatement -> prPrec i 0 (concatD [prt 0 wstatement])
    Parser.AbsGrammar.Block wstatements -> prPrec i 0 (concatD [doc (showString "{"), prt 0 wstatements, doc (showString "}")])

instance Print Parser.AbsGrammar.WExpression where
  prt i e = case e of
    Parser.AbsGrammar.WTry wblockorexpression wcatchs wthenalways -> prPrec i 0 (concatD [doc (showString "try"), prt 0 wblockorexpression, prt 0 wcatchs, prt 0 wthenalways])
    Parser.AbsGrammar.WOrExpression wexpression1 opor wexpression2 -> prPrec i 1 (concatD [prt 0 wexpression1, prt 0 opor, prt 1 wexpression2])
    Parser.AbsGrammar.WAndExpression wexpression1 opand wexpression2 -> prPrec i 1 (concatD [prt 1 wexpression1, prt 0 opand, prt 2 wexpression2])
    Parser.AbsGrammar.WEqExpression wexpression1 opeq wexpression2 -> prPrec i 2 (concatD [prt 2 wexpression1, prt 0 opeq, prt 3 wexpression2])
    Parser.AbsGrammar.WCmpExpression wexpression1 opcmp wexpression2 -> prPrec i 3 (concatD [prt 3 wexpression1, prt 0 opcmp, prt 4 wexpression2])
    Parser.AbsGrammar.WAddExpression wexpression1 opadd wexpression2 -> prPrec i 4 (concatD [prt 4 wexpression1, prt 0 opadd, prt 5 wexpression2])
    Parser.AbsGrammar.WMultExpression wexpression1 opmult wexpression2 -> prPrec i 5 (concatD [prt 5 wexpression1, prt 0 opmult, prt 6 wexpression2])
    Parser.AbsGrammar.WPowerExpression wexpression1 oppower wexpression2 -> prPrec i 6 (concatD [prt 7 wexpression1, prt 0 oppower, prt 6 wexpression2])
    Parser.AbsGrammar.WUnaryExpression opunary wexpression -> prPrec i 7 (concatD [prt 0 opunary, prt 8 wexpression])
    Parser.AbsGrammar.WPostfixExpression wexpression oppostfix -> prPrec i 8 (concatD [prt 9 wexpression, prt 0 oppostfix])
    Parser.AbsGrammar.WMessageSend wexpression id wexpressions -> prPrec i 9 (concatD [prt 9 wexpression, doc (showString "."), prt 0 id, doc (showString "("), prt 0 wexpressions, doc (showString ")")])
    Parser.AbsGrammar.WClosure wclosureparameters wstatements -> prPrec i 10 (concatD [doc (showString "{"), prt 0 wclosureparameters, prt 0 wstatements, doc (showString "}")])
    Parser.AbsGrammar.WIf wexpression wblockorexpression welse -> prPrec i 10 (concatD [doc (showString "if"), doc (showString "("), prt 0 wexpression, doc (showString ")"), prt 0 wblockorexpression, prt 0 welse])
    Parser.AbsGrammar.WObjectLiteral id wsuperclassdeclaration wvariabledeclarations wmethoddeclarations -> prPrec i 10 (concatD [doc (showString "object"), prt 0 id, prt 0 wsuperclassdeclaration, doc (showString "{"), prt 0 wvariabledeclarations, prt 0 wmethoddeclarations, doc (showString "}")])
    Parser.AbsGrammar.WNumberLiteral n -> prPrec i 10 (concatD [prt 0 n])
    Parser.AbsGrammar.WNullLiteral -> prPrec i 10 (concatD [doc (showString "null")])
    Parser.AbsGrammar.WLiteralTrue -> prPrec i 10 (concatD [doc (showString "true")])
    Parser.AbsGrammar.WLiteralFalse -> prPrec i 10 (concatD [doc (showString "false")])
    Parser.AbsGrammar.WSelf -> prPrec i 10 (concatD [doc (showString "self")])
    Parser.AbsGrammar.WStringLiteral str -> prPrec i 10 (concatD [prt 0 str])
    Parser.AbsGrammar.WVariable id -> prPrec i 10 (concatD [prt 0 id])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Parser.AbsGrammar.WCatch] where
  prt = prtList

instance Print Parser.AbsGrammar.WCatch where
  prt i e = case e of
    Parser.AbsGrammar.WCatch id exceptiontype wblockorexpression -> prPrec i 0 (concatD [doc (showString "catch"), prt 0 id, prt 0 exceptiontype, prt 0 wblockorexpression])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.AbsGrammar.WThenAlways where
  prt i e = case e of
    Parser.AbsGrammar.WThenAlwaysProvided wblockorexpression -> prPrec i 0 (concatD [doc (showString "then"), doc (showString "always"), prt 0 wblockorexpression])
    Parser.AbsGrammar.WNoThenAlways -> prPrec i 0 (concatD [])

instance Print Parser.AbsGrammar.ExceptionType where
  prt i e = case e of
    Parser.AbsGrammar.ProvidedExceptionType id -> prPrec i 0 (concatD [doc (showString ":"), prt 0 id])
    Parser.AbsGrammar.DefaultExceptionType -> prPrec i 0 (concatD [])

instance Print Parser.AbsGrammar.OpOr where
  prt i e = case e of
    Parser.AbsGrammar.OpOr1 -> prPrec i 0 (concatD [doc (showString "||")])
    Parser.AbsGrammar.OpOr_or -> prPrec i 0 (concatD [doc (showString "or")])

instance Print Parser.AbsGrammar.OpAnd where
  prt i e = case e of
    Parser.AbsGrammar.OpAnd1 -> prPrec i 0 (concatD [doc (showString "&&")])
    Parser.AbsGrammar.OpAnd_and -> prPrec i 0 (concatD [doc (showString "and")])

instance Print Parser.AbsGrammar.OpEq where
  prt i e = case e of
    Parser.AbsGrammar.OpEq1 -> prPrec i 0 (concatD [doc (showString "==")])
    Parser.AbsGrammar.OpEq2 -> prPrec i 0 (concatD [doc (showString "!=")])
    Parser.AbsGrammar.OpEq3 -> prPrec i 0 (concatD [doc (showString "===")])
    Parser.AbsGrammar.OpEq4 -> prPrec i 0 (concatD [doc (showString "!==")])

instance Print Parser.AbsGrammar.OpCmp where
  prt i e = case e of
    Parser.AbsGrammar.OpCmp1 -> prPrec i 0 (concatD [doc (showString ">=")])
    Parser.AbsGrammar.OpCmp2 -> prPrec i 0 (concatD [doc (showString "<=")])
    Parser.AbsGrammar.OpCmp3 -> prPrec i 0 (concatD [doc (showString ">")])
    Parser.AbsGrammar.OpCmp4 -> prPrec i 0 (concatD [doc (showString "<")])

instance Print Parser.AbsGrammar.OpAdd where
  prt i e = case e of
    Parser.AbsGrammar.OpAdd1 -> prPrec i 0 (concatD [doc (showString "+")])
    Parser.AbsGrammar.OpAdd2 -> prPrec i 0 (concatD [doc (showString "-")])

instance Print Parser.AbsGrammar.OpMult where
  prt i e = case e of
    Parser.AbsGrammar.OpMult1 -> prPrec i 0 (concatD [doc (showString "*")])
    Parser.AbsGrammar.OpMult2 -> prPrec i 0 (concatD [doc (showString "/")])
    Parser.AbsGrammar.OpMult3 -> prPrec i 0 (concatD [doc (showString "%")])

instance Print Parser.AbsGrammar.OpPower where
  prt i e = case e of
    Parser.AbsGrammar.OpPower1 -> prPrec i 0 (concatD [doc (showString "**")])

instance Print Parser.AbsGrammar.OpUnary where
  prt i e = case e of
    Parser.AbsGrammar.OpUnary_not -> prPrec i 0 (concatD [doc (showString "not")])
    Parser.AbsGrammar.OpUnary1 -> prPrec i 0 (concatD [doc (showString "!")])
    Parser.AbsGrammar.OpUnary2 -> prPrec i 0 (concatD [doc (showString "-")])
    Parser.AbsGrammar.OpUnary3 -> prPrec i 0 (concatD [doc (showString "+")])

instance Print Parser.AbsGrammar.OpPostfix where
  prt i e = case e of
    Parser.AbsGrammar.OpPostfix1 -> prPrec i 0 (concatD [doc (showString "++")])
    Parser.AbsGrammar.OpPostfix2 -> prPrec i 0 (concatD [doc (showString "--")])

instance Print [Parser.AbsGrammar.WExpression] where
  prt = prtList

instance Print Parser.AbsGrammar.WClosureParameters where
  prt i e = case e of
    Parser.AbsGrammar.WNoParameters -> prPrec i 0 (concatD [])
    Parser.AbsGrammar.WWithParameters ids -> prPrec i 0 (concatD [prt 0 ids, doc (showString "=>")])

instance Print Parser.AbsGrammar.WElse where
  prt i e = case e of
    Parser.AbsGrammar.WNoElse -> prPrec i 0 (concatD [])
    Parser.AbsGrammar.WElse wblockorexpression -> prPrec i 0 (concatD [doc (showString "else"), prt 0 wblockorexpression])

