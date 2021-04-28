{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.ParGrammar where
import qualified Parser.AbsGrammar
import Parser.LexGrammar
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap24 = HappyWrap24 (Parser.AbsGrammar.Ident)
happyIn24 :: (Parser.AbsGrammar.Ident) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (Integer)
happyIn25 :: (Integer) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (String)
happyIn26 :: (String) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (Parser.AbsGrammar.WFile)
happyIn27 :: (Parser.AbsGrammar.WFile) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 ([Parser.AbsGrammar.Import])
happyIn28 :: ([Parser.AbsGrammar.Import]) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 ([Parser.AbsGrammar.WLibraryElement])
happyIn29 :: ([Parser.AbsGrammar.WLibraryElement]) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (Parser.AbsGrammar.Import)
happyIn30 :: (Parser.AbsGrammar.Import) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (Parser.AbsGrammar.WLibraryElement)
happyIn31 :: (Parser.AbsGrammar.WLibraryElement) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (Parser.AbsGrammar.WProgram)
happyIn32 :: (Parser.AbsGrammar.WProgram) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ([Parser.AbsGrammar.WStatement])
happyIn33 :: ([Parser.AbsGrammar.WStatement]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 (Parser.AbsGrammar.WClassDeclaration)
happyIn34 :: (Parser.AbsGrammar.WClassDeclaration) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 ([Parser.AbsGrammar.WVariableDeclaration])
happyIn35 :: ([Parser.AbsGrammar.WVariableDeclaration]) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 ([Parser.AbsGrammar.WMethodDeclaration])
happyIn36 :: ([Parser.AbsGrammar.WMethodDeclaration]) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (Parser.AbsGrammar.WSuperclassDeclaration)
happyIn37 :: (Parser.AbsGrammar.WSuperclassDeclaration) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (Parser.AbsGrammar.WMethodDeclaration)
happyIn38 :: (Parser.AbsGrammar.WMethodDeclaration) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 ([Parser.AbsGrammar.Ident])
happyIn39 :: ([Parser.AbsGrammar.Ident]) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (Parser.AbsGrammar.NativeIndicator)
happyIn40 :: (Parser.AbsGrammar.NativeIndicator) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (Parser.AbsGrammar.MethodBody)
happyIn41 :: (Parser.AbsGrammar.MethodBody) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (Parser.AbsGrammar.WStatement)
happyIn42 :: (Parser.AbsGrammar.WStatement) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (Parser.AbsGrammar.WVariableDeclaration)
happyIn43 :: (Parser.AbsGrammar.WVariableDeclaration) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (Parser.AbsGrammar.WVariableType)
happyIn44 :: (Parser.AbsGrammar.WVariableType) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (Parser.AbsGrammar.WExpression)
happyIn45 :: (Parser.AbsGrammar.WExpression) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ([Parser.AbsGrammar.WExpression])
happyIn46 :: ([Parser.AbsGrammar.WExpression]) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (Parser.AbsGrammar.WExpression)
happyIn47 :: (Parser.AbsGrammar.WExpression) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x80\x80\xa1\x73\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x02\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x80\x80\xa1\x73\x00\x00\x00\x00\x00\x00\x80\x00\x02\x00\x00\x00\x00\x00\x00\x80\x00\x02\x00\x00\x00\x00\x00\x80\x00\xa1\x71\x00\x00\x00\x00\x00\x80\x00\xa1\x71\x00\x00\x00\x00\x00\x80\x00\xa1\x71\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\xa1\x71\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x80\xa1\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x80\xa1\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x80\x00\xa1\x71\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\xa1\x71\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x80\x80\xa1\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x80\x00\x02\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x80\x00\xa1\x71\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pWFile","%start_pListImport","%start_pListWLibraryElement","%start_pImport","%start_pWLibraryElement","%start_pWProgram","%start_pListWStatement","%start_pWClassDeclaration","%start_pListWVariableDeclaration","%start_pListWMethodDeclaration","%start_pWSuperclassDeclaration","%start_pWMethodDeclaration","%start_pListIdent","%start_pNativeIndicator","%start_pMethodBody","%start_pWStatement","%start_pWVariableDeclaration","%start_pWVariableType","%start_pWExpression","%start_pListWExpression","%start_pWExpression1","Ident","Integer","String","WFile","ListImport","ListWLibraryElement","Import","WLibraryElement","WProgram","ListWStatement","WClassDeclaration","ListWVariableDeclaration","ListWMethodDeclaration","WSuperclassDeclaration","WMethodDeclaration","ListIdent","NativeIndicator","MethodBody","WStatement","WVariableDeclaration","WVariableType","WExpression","ListWExpression","WExpression1","'('","')'","','","'-'","'.'","'='","'TODO'","'class'","'const'","'false'","'import'","'inherits'","'method'","'native'","'null'","'program'","'self'","'true'","'var'","'{'","'}'","L_Ident","L_integ","L_quoted","%eof"]
        bit_start = st * 72
        bit_end = (st + 1) * 72
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..71]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x06\x00\x06\x00\x04\x00\x11\x00\x25\x00\xf9\xff\x14\x00\x36\x00\x2c\x00\x39\x00\x4c\x00\x3f\x00\x4e\x00\x62\x00\x5d\x00\x14\x00\x2c\x00\x2c\x00\x54\x00\x54\x00\x54\x00\x65\x00\x00\x00\x00\x00\x00\x00\x00\x00\x72\x00\x54\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\x00\x72\x00\x00\x00\x02\x00\x72\x00\x00\x00\x00\x00\x72\x00\x7b\x00\x79\x00\x00\x00\x01\x00\x79\x00\x14\x00\x79\x00\x00\x00\x73\x00\x99\x00\x99\x00\x90\x00\x9b\x00\xa0\x00\xa1\x00\xaa\x00\xa3\x00\x2c\x00\xa3\x00\xa5\x00\xa4\x00\x14\x00\xa4\x00\xa8\x00\xa9\x00\x00\x00\xa9\x00\xb4\x00\xac\x00\xb1\x00\xad\x00\xb5\x00\xae\x00\xb7\x00\xb3\x00\x00\x00\x00\x00\xba\x00\xb0\x00\x00\x00\xbc\x00\x00\x00\x00\x00\x00\x00\xc8\x00\xb6\x00\xb8\x00\xb9\x00\xc4\x00\x54\x00\xff\xff\x00\x00\x00\x00\x54\x00\xca\x00\x00\x00\x00\x00\xbb\x00\xbe\x00\x14\x00\x00\x00\x00\x00\xbf\x00\x2c\x00\xcc\x00\x54\x00\xcb\x00\xd1\x00\xc7\x00\xc9\x00\x00\x00\xc2\x00\xc5\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xa6\x00\x53\x00\xfe\xff\xd2\x00\x89\x00\xd3\x00\x1f\x00\xd0\x00\x40\x00\x9f\x00\xcf\x00\xd4\x00\x29\x00\xcd\x00\xce\x00\x60\x00\x26\x00\xd5\x00\x0d\x00\x67\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x00\x00\xe1\x00\x00\x00\xa2\x00\x00\x00\x8a\x00\x00\x00\xe3\x00\x00\x00\x3b\x00\x00\x00\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x00\x00\xab\x00\x00\x00\x9e\x00\xdd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x00\x00\xe7\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x00\x00\x85\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x8d\x00\x00\x00\x78\x00\x00\x00\x00\x00\xd8\x00\xa7\x00\x00\x00\x00\x00\xda\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xe6\xff\xe6\xff\xe4\xff\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\xdc\xff\xda\xff\xd7\xff\x00\x00\xd5\xff\xd1\xff\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\x00\x00\x00\x00\xea\xff\xbd\xff\xc3\xff\xbe\xff\x00\x00\x00\x00\xc0\xff\xc2\xff\xbf\xff\xc1\xff\xe9\xff\xe8\xff\xc5\xff\x00\x00\xc7\xff\x00\x00\x00\x00\xc9\xff\xca\xff\x00\x00\x00\x00\x00\x00\xcd\xff\xcc\xff\x00\x00\xdf\xff\x00\x00\xd2\xff\xd4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\x00\x00\xdc\xff\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\x00\x00\x00\x00\xe1\xff\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\xe6\xff\x00\x00\xe4\xff\x00\x00\xe5\xff\xe3\xff\x00\x00\x00\x00\xde\xff\xd7\xff\xdb\xff\xd9\xff\xd8\xff\x00\x00\xd5\xff\x00\x00\x00\x00\x00\x00\xc6\xff\x00\x00\xbc\xff\xc4\xff\x00\x00\x00\x00\xd0\xff\xd3\xff\xd5\xff\x00\x00\xdf\xff\xe2\xff\xe7\xff\x00\x00\xdc\xff\x00\x00\xc6\xff\xcb\xff\x00\x00\xd1\xff\xda\xff\xe0\xff\x00\x00\xcf\xff\xc8\xff\xd6\xff\xdd\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x05\x00\x05\x00\x07\x00\x05\x00\x05\x00\x0a\x00\x10\x00\x09\x00\x0a\x00\x08\x00\x00\x00\x01\x00\x02\x00\x0f\x00\x0b\x00\x11\x00\x12\x00\x13\x00\x01\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x19\x00\x0b\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x15\x00\x0f\x00\x17\x00\x11\x00\x12\x00\x13\x00\x09\x00\x00\x00\x16\x00\x17\x00\x18\x00\x08\x00\x00\x00\x01\x00\x02\x00\x12\x00\x13\x00\x14\x00\x15\x00\x09\x00\x17\x00\x09\x00\x0f\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x08\x00\x13\x00\x12\x00\x13\x00\x14\x00\x15\x00\x09\x00\x17\x00\x0d\x00\x00\x00\x00\x00\x01\x00\x02\x00\x0b\x00\x0d\x00\x12\x00\x13\x00\x14\x00\x15\x00\x09\x00\x17\x00\x13\x00\x14\x00\x01\x00\x0f\x00\x04\x00\x0c\x00\x06\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0a\x00\x17\x00\x00\x00\x01\x00\x02\x00\x0f\x00\x16\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x0e\x00\x14\x00\x12\x00\x13\x00\x14\x00\x15\x00\x03\x00\x17\x00\x00\x00\x01\x00\x02\x00\x16\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x19\x00\x00\x00\x15\x00\x16\x00\x17\x00\x07\x00\x16\x00\x19\x00\x0a\x00\x15\x00\x0b\x00\x17\x00\x03\x00\x0b\x00\x05\x00\x15\x00\x0f\x00\x17\x00\x13\x00\x14\x00\x17\x00\x13\x00\x14\x00\x05\x00\x05\x00\x07\x00\x07\x00\x16\x00\x0a\x00\x0a\x00\x03\x00\x04\x00\x0c\x00\x06\x00\x0e\x00\x0c\x00\x04\x00\x0e\x00\x06\x00\x19\x00\x0c\x00\x19\x00\x0e\x00\x16\x00\x0d\x00\x04\x00\x08\x00\x19\x00\x16\x00\x19\x00\x19\x00\x16\x00\x08\x00\x0b\x00\x07\x00\x19\x00\x10\x00\x14\x00\x19\x00\x19\x00\x19\x00\x0c\x00\x01\x00\x06\x00\x01\x00\x16\x00\x15\x00\x02\x00\x16\x00\x05\x00\x16\x00\x14\x00\x02\x00\x15\x00\x0e\x00\x0d\x00\x15\x00\x06\x00\x14\x00\x0a\x00\x08\x00\x0d\x00\x10\x00\x00\x00\x11\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x08\x00\x0d\x00\x00\x00\x10\x00\x14\x00\xff\xff\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x5e\x00\xcc\xff\x46\x00\x5a\x00\x47\x00\x5a\x00\x5a\x00\x43\x00\x42\x00\xcc\xff\xcc\xff\x3e\x00\x17\x00\x18\x00\x19\x00\xcc\xff\x46\x00\xcc\xff\xcc\xff\xcc\xff\x1c\x00\xcc\xff\xcc\xff\xcc\xff\xcc\xff\xcc\xff\xff\xff\x46\x00\x28\x00\x1d\x00\x17\x00\x18\x00\x19\x00\x25\x00\x1e\x00\x24\x00\x1f\x00\x20\x00\x29\x00\x3e\x00\x32\x00\x17\x00\x21\x00\x22\x00\x3e\x00\x17\x00\x18\x00\x19\x00\x3f\x00\x2c\x00\x2a\x00\x2d\x00\x28\x00\x24\x00\x58\x00\x33\x00\x29\x00\x2a\x00\x17\x00\x18\x00\x19\x00\x3e\x00\x29\x00\x3f\x00\x2c\x00\x2a\x00\x2d\x00\x51\x00\x24\x00\x36\x00\x32\x00\x17\x00\x18\x00\x19\x00\x3a\x00\x36\x00\x3f\x00\x2c\x00\x2a\x00\x2d\x00\x68\x00\x24\x00\x3b\x00\x2a\x00\x1c\x00\x62\x00\x48\x00\x38\x00\x49\x00\x3f\x00\x2c\x00\x2a\x00\x2d\x00\x1d\x00\x24\x00\x17\x00\x18\x00\x19\x00\x1e\x00\x17\x00\x1f\x00\x20\x00\x17\x00\x18\x00\x19\x00\x17\x00\x21\x00\x22\x00\x17\x00\x18\x00\x19\x00\x32\x00\x30\x00\x2b\x00\x2c\x00\x2a\x00\x2d\x00\x58\x00\x24\x00\x17\x00\x18\x00\x19\x00\x17\x00\x22\x00\x23\x00\x24\x00\x17\x00\x18\x00\x19\x00\x22\x00\x5e\x00\x24\x00\x17\x00\x18\x00\x19\x00\x17\x00\x18\x00\x19\x00\xff\xff\x32\x00\x22\x00\x6d\x00\x24\x00\x42\x00\x17\x00\xff\xff\x43\x00\x5c\x00\x53\x00\x24\x00\x5c\x00\x6f\x00\x5a\x00\x6c\x00\x6a\x00\x24\x00\x3b\x00\x2a\x00\x1a\x00\x3b\x00\x2a\x00\x4e\x00\x4c\x00\x47\x00\x47\x00\x17\x00\x43\x00\x43\x00\x4a\x00\x4b\x00\x38\x00\x49\x00\x39\x00\x54\x00\x4d\x00\x39\x00\x49\x00\xff\xff\x71\x00\xff\xff\x39\x00\x17\x00\x36\x00\x50\x00\x3e\x00\xff\xff\x17\x00\xff\xff\xff\xff\x17\x00\x3e\x00\x46\x00\x67\x00\xff\xff\x42\x00\x66\x00\xff\xff\xff\xff\xff\xff\x38\x00\x64\x00\x60\x00\x6c\x00\x17\x00\x62\x00\x6f\x00\x17\x00\x5a\x00\x17\x00\x6a\x00\x74\x00\x71\x00\x32\x00\x36\x00\x76\x00\x44\x00\x30\x00\x3c\x00\x40\x00\x36\x00\x30\x00\x5a\x00\x2e\x00\x56\x00\x55\x00\x34\x00\x52\x00\x50\x00\x67\x00\x64\x00\x60\x00\x72\x00\x26\x00\x00\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (21, 67) [
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67)
	]

happy_n_terms = 26 :: Int
happy_n_nonterms = 24 :: Int

happyReduce_21 = happySpecReduce_1  0# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn24
		 (Parser.AbsGrammar.Ident happy_var_1
	)}

happyReduce_22 = happySpecReduce_1  1# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn25
		 ((read (happy_var_1)) :: Integer
	)}

happyReduce_23 = happySpecReduce_1  2# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn26
		 (happy_var_1
	)}

happyReduce_24 = happySpecReduce_3  3# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	case happyOut32 happy_x_3 of { (HappyWrap32 happy_var_3) -> 
	happyIn27
		 (Parser.AbsGrammar.WFile happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_25 = happySpecReduce_0  4# happyReduction_25
happyReduction_25  =  happyIn28
		 ([]
	)

happyReduce_26 = happySpecReduce_2  4# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	happyIn28
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_27 = happySpecReduce_0  5# happyReduction_27
happyReduction_27  =  happyIn29
		 ([]
	)

happyReduce_28 = happySpecReduce_2  5# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	happyIn29
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_29 = happySpecReduce_3  6# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn30
		 (Parser.AbsGrammar.Import
	)

happyReduce_30 = happySpecReduce_1  7# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn31
		 (Parser.AbsGrammar.WLibraryElement happy_var_1
	)}

happyReduce_31 = happyReduce 5# 8# happyReduction_31
happyReduction_31 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	case happyOut33 happy_x_4 of { (HappyWrap33 happy_var_4) -> 
	happyIn32
		 (Parser.AbsGrammar.WProgram happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_32 = happySpecReduce_0  9# happyReduction_32
happyReduction_32  =  happyIn33
		 ([]
	)

happyReduce_33 = happySpecReduce_2  9# happyReduction_33
happyReduction_33 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	happyIn33
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_34 = happyReduce 7# 10# happyReduction_34
happyReduction_34 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	case happyOut37 happy_x_3 of { (HappyWrap37 happy_var_3) -> 
	case happyOut35 happy_x_5 of { (HappyWrap35 happy_var_5) -> 
	case happyOut36 happy_x_6 of { (HappyWrap36 happy_var_6) -> 
	happyIn34
		 (Parser.AbsGrammar.WClassDeclaration happy_var_2 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_35 = happySpecReduce_0  11# happyReduction_35
happyReduction_35  =  happyIn35
		 ([]
	)

happyReduce_36 = happySpecReduce_2  11# happyReduction_36
happyReduction_36 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	happyIn35
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_37 = happySpecReduce_0  12# happyReduction_37
happyReduction_37  =  happyIn36
		 ([]
	)

happyReduce_38 = happySpecReduce_2  12# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut36 happy_x_2 of { (HappyWrap36 happy_var_2) -> 
	happyIn36
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_39 = happySpecReduce_2  13# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn37
		 (Parser.AbsGrammar.WSuperclass happy_var_2
	)}

happyReduce_40 = happySpecReduce_0  13# happyReduction_40
happyReduction_40  =  happyIn37
		 (Parser.AbsGrammar.WNoSuperclass
	)

happyReduce_41 = happyReduce 7# 14# happyReduction_41
happyReduction_41 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	case happyOut39 happy_x_4 of { (HappyWrap39 happy_var_4) -> 
	case happyOut40 happy_x_6 of { (HappyWrap40 happy_var_6) -> 
	case happyOut41 happy_x_7 of { (HappyWrap41 happy_var_7) -> 
	happyIn38
		 (Parser.AbsGrammar.WMethodDeclaration happy_var_2 happy_var_4 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_42 = happySpecReduce_0  15# happyReduction_42
happyReduction_42  =  happyIn39
		 ([]
	)

happyReduce_43 = happySpecReduce_1  15# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn39
		 ((:[]) happy_var_1
	)}

happyReduce_44 = happySpecReduce_3  15# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut39 happy_x_3 of { (HappyWrap39 happy_var_3) -> 
	happyIn39
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_1  16# happyReduction_45
happyReduction_45 happy_x_1
	 =  happyIn40
		 (Parser.AbsGrammar.Native
	)

happyReduce_46 = happySpecReduce_0  16# happyReduction_46
happyReduction_46  =  happyIn40
		 (Parser.AbsGrammar.Custom
	)

happyReduce_47 = happySpecReduce_3  17# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	happyIn41
		 (Parser.AbsGrammar.Implemented happy_var_2
	)}

happyReduce_48 = happySpecReduce_0  17# happyReduction_48
happyReduction_48  =  happyIn41
		 (Parser.AbsGrammar.NotImplemented
	)

happyReduce_49 = happySpecReduce_1  18# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn42
		 (Parser.AbsGrammar.TopLevelExpression happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  18# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn42
		 (Parser.AbsGrammar.VarDeclaration happy_var_1
	)}

happyReduce_51 = happySpecReduce_1  18# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn42
		 (Parser.AbsGrammar.TopLevelExpression happy_var_1
	)}

happyReduce_52 = happyReduce 4# 19# happyReduction_52
happyReduction_52 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	case happyOut45 happy_x_4 of { (HappyWrap45 happy_var_4) -> 
	happyIn43
		 (Parser.AbsGrammar.WVariableDeclaration happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_53 = happySpecReduce_1  20# happyReduction_53
happyReduction_53 happy_x_1
	 =  happyIn44
		 (Parser.AbsGrammar.Var
	)

happyReduce_54 = happySpecReduce_1  20# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn44
		 (Parser.AbsGrammar.Const
	)

happyReduce_55 = happyReduce 6# 21# happyReduction_55
happyReduction_55 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut46 happy_x_5 of { (HappyWrap46 happy_var_5) -> 
	happyIn45
		 (Parser.AbsGrammar.WMessageSend happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_56 = happySpecReduce_1  21# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_57 = happySpecReduce_0  22# happyReduction_57
happyReduction_57  =  happyIn46
		 ([]
	)

happyReduce_58 = happySpecReduce_1  22# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn46
		 ((:[]) happy_var_1
	)}

happyReduce_59 = happySpecReduce_3  22# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut46 happy_x_3 of { (HappyWrap46 happy_var_3) -> 
	happyIn46
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_60 = happySpecReduce_1  23# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn47
		 (Parser.AbsGrammar.WNumberLiteral happy_var_1
	)}

happyReduce_61 = happySpecReduce_1  23# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn47
		 (Parser.AbsGrammar.WNullLiteral
	)

happyReduce_62 = happySpecReduce_1  23# happyReduction_62
happyReduction_62 happy_x_1
	 =  happyIn47
		 (Parser.AbsGrammar.WLiteralTrue
	)

happyReduce_63 = happySpecReduce_1  23# happyReduction_63
happyReduction_63 happy_x_1
	 =  happyIn47
		 (Parser.AbsGrammar.WLiteralFalse
	)

happyReduce_64 = happySpecReduce_1  23# happyReduction_64
happyReduction_64 happy_x_1
	 =  happyIn47
		 (Parser.AbsGrammar.WSelf
	)

happyReduce_65 = happySpecReduce_1  23# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn47
		 (Parser.AbsGrammar.WStringLiteral happy_var_1
	)}

happyReduce_66 = happySpecReduce_1  23# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn47
		 (Parser.AbsGrammar.WVariable happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  23# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { (HappyWrap45 happy_var_2) -> 
	happyIn47
		 (happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 25# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TV happy_dollar_dollar) -> cont 22#;
	PT _ (TI happy_dollar_dollar) -> cont 23#;
	PT _ (TL happy_dollar_dollar) -> cont 24#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 25# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = ((>>=))
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Either String a
happyError' = (\(tokens, _) -> happyError tokens)
pWFile tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap27 x') = happyOut27 x} in x'))

pListImport tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap28 x') = happyOut28 x} in x'))

pListWLibraryElement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap29 x') = happyOut29 x} in x'))

pImport tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap30 x') = happyOut30 x} in x'))

pWLibraryElement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap31 x') = happyOut31 x} in x'))

pWProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap32 x') = happyOut32 x} in x'))

pListWStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap33 x') = happyOut33 x} in x'))

pWClassDeclaration tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap34 x') = happyOut34 x} in x'))

pListWVariableDeclaration tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap35 x') = happyOut35 x} in x'))

pListWMethodDeclaration tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap36 x') = happyOut36 x} in x'))

pWSuperclassDeclaration tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap37 x') = happyOut37 x} in x'))

pWMethodDeclaration tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap38 x') = happyOut38 x} in x'))

pListIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap39 x') = happyOut39 x} in x'))

pNativeIndicator tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap40 x') = happyOut40 x} in x'))

pMethodBody tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap41 x') = happyOut41 x} in x'))

pWStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap42 x') = happyOut42 x} in x'))

pWVariableDeclaration tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap43 x') = happyOut43 x} in x'))

pWVariableType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pWExpression tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pListWExpression tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pWExpression1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

happySeq = happyDontSeq


happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
