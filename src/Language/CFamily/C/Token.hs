{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Parser.Tokens
-- Copyright   :  [1999..2004] Manuel M T Chakravarty
--                2005 Duncan Coutts
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
--  C Tokens for the C lexer.
--
-----------------------------------------------------------------------------
module Language.CFamily.C.Token (Token(..), posLenOfTok, GnuTok(..)) where

import Language.CFamily.Data.Position    (Position, Pos(..), PosLength)
import Language.CFamily.Data.Ident       (Ident, identToString)
import Language.CFamily.C.Constants (CChar, CInteger, CFloat, CString)

-- token definition
-- ----------------

-- possible tokens (EXPORTED)
--
data Token = TokLParen   !PosLength            -- `('
            | TokRParen   !PosLength            -- `)'
            | TokLBracket !PosLength            -- `['
            | TokRBracket !PosLength            -- `]'
            | TokArrow    !PosLength            -- `->'
            | TokDot      !PosLength            -- `.'
            | TokExclam   !PosLength            -- `!'
            | TokTilde    !PosLength            -- `~'
            | TokInc      !PosLength            -- `++'
            | TokDec      !PosLength            -- `--'
            | TokPlus     !PosLength            -- `+'
            | TokMinus    !PosLength            -- `-'
            | TokStar     !PosLength            -- `*'
            | TokSlash    !PosLength            -- `/'
            | TokPercent  !PosLength            -- `%'
            | TokAmper    !PosLength            -- `&'
            | TokShiftL   !PosLength            -- `<<'
            | TokShiftR   !PosLength            -- `>>'
            | TokLess     !PosLength            -- `<'
            | TokLessEq   !PosLength            -- `<='
            | TokHigh     !PosLength            -- `>'
            | TokHighEq   !PosLength            -- `>='
            | TokEqual    !PosLength            -- `=='
            | TokUnequal  !PosLength            -- `!='
            | TokHat      !PosLength            -- `^'
            | TokBar      !PosLength            -- `|'
            | TokAnd      !PosLength            -- `&&'
            | TokOr       !PosLength            -- `||'
            | TokQuest    !PosLength            -- `?'
            | TokColon    !PosLength            -- `:'
            | TokAssign   !PosLength            -- `='
            | TokPlusAss  !PosLength            -- `+='
            | TokMinusAss !PosLength            -- `-='
            | TokStarAss  !PosLength            -- `*='
            | TokSlashAss !PosLength            -- `/='
            | TokPercAss  !PosLength            -- `%='
            | TokAmpAss   !PosLength            -- `&='
            | TokHatAss   !PosLength            -- `^='
            | TokBarAss   !PosLength            -- `|='
            | TokSLAss    !PosLength            -- `<<='
            | TokSRAss    !PosLength            -- `>>='
            | TokComma    !PosLength            -- `,'
            | TokSemic    !PosLength            -- `;'
            | TokLBrace   !PosLength            -- `{'
            | TokRBrace   !PosLength            -- `}'
            | TokEllipsis !PosLength            -- `...'
            | TokAlignof  !PosLength            -- `alignof'
                                                -- (or `__alignof',
                                                -- `__alignof__')
            | TokAsm      !PosLength            -- `asm'
                                                -- (or `__asm',
                                                -- `__asm__')
            | TokAuto     !PosLength            -- `auto'
            | TokBreak    !PosLength            -- `break'
            | TokBool     !PosLength            -- `_Bool'
            | TokCase     !PosLength            -- `case'
            | TokChar     !PosLength            -- `char'
            | TokConst    !PosLength            -- `const'
                                                -- (or `__const', `__const__')
            | TokContinue !PosLength            -- `continue'
            | TokComplex  !PosLength            -- `_Complex'
            | TokDefault  !PosLength            -- `default'
            | TokDo       !PosLength            -- `do'
            | TokDouble   !PosLength            -- `double'
            | TokElse     !PosLength            -- `else'
            | TokEnum     !PosLength            -- `enum'
            | TokExtern   !PosLength            -- `extern'
            | TokFloat    !PosLength            -- `float'
            | TokFor      !PosLength            -- `for'
            | TokGoto     !PosLength            -- `goto'
            | TokIf       !PosLength            -- `if'
            | TokInline   !PosLength            -- `inline'
                                                -- (or `__inline',
                                                -- `__inline__')
            | TokInt      !PosLength            -- `int'
            | TokLong     !PosLength            -- `long'
            | TokLabel    !PosLength            -- `__label__'
            | TokRegister !PosLength            -- `register'
            | TokRestrict !PosLength            -- `restrict'
                                                -- (or `__restrict',
                                                -- `__restrict__')
            | TokReturn   !PosLength            -- `return'
            | TokShort    !PosLength            -- `short'
            | TokSigned   !PosLength            -- `signed'
                                                -- (or `__signed',
                                                -- `__signed__')
            | TokSizeof   !PosLength            -- `sizeof'
            | TokStatic   !PosLength            -- `static'
            | TokStruct   !PosLength            -- `struct'
            | TokSwitch   !PosLength            -- `switch'
            | TokTypedef  !PosLength            -- `typedef'
            | TokTypeof   !PosLength            -- `typeof'
            | TokThread   !PosLength            -- `__thread'
            | TokUnion    !PosLength            -- `union'
            | TokUnsigned !PosLength            -- `unsigned'
            | TokVoid     !PosLength            -- `void'
            | TokVolatile !PosLength            -- `volatile'
                                                -- (or `__volatile',
                                                -- `__volatile__')
            | TokWhile    !PosLength            -- `while'
            | TokCLit     !PosLength !CChar     -- character constant
            | TokILit     !PosLength !CInteger  -- integer constant
            | TokFLit     !PosLength CFloat     -- float constant
            | TokSLit     !PosLength CString    -- string constant
            | TokIdent    !PosLength !Ident     -- identifier

              -- not generated here, but in `CParser.parseCHeader'
            | TokTyIdent  !PosLength !Ident     -- `typedef-name' identifier
            | TokGnuC !GnuTok !PosLength       -- special GNU C tokens
            | TokEof                           -- end of file

-- special tokens used in GNU C extensions to ANSI C
--
data GnuTok = GnuCAttrTok              -- `__attribute__'
             | GnuCExtTok               -- `__extension__'
             | GnuCVaArg                -- `__builtin_va_arg'
             | GnuCOffsetof             -- `__builtin_offsetof'
             | GnuCTyCompat             -- `__builtin_types_compatible_p'
             | GnuCComplexReal          -- `__real__'
             | GnuCComplexImag          -- `__imag__'

instance Pos Token where
  posOf = fst . posLenOfTok

-- token position and length
posLenOfTok :: Token -> (Position,Int)
posLenOfTok (TokLParen   pos  ) = pos
posLenOfTok (TokRParen   pos  ) = pos
posLenOfTok (TokLBracket pos  ) = pos
posLenOfTok (TokRBracket pos  ) = pos
posLenOfTok (TokArrow    pos  ) = pos
posLenOfTok (TokDot      pos  ) = pos
posLenOfTok (TokExclam   pos  ) = pos
posLenOfTok (TokTilde    pos  ) = pos
posLenOfTok (TokInc      pos  ) = pos
posLenOfTok (TokDec      pos  ) = pos
posLenOfTok (TokPlus     pos  ) = pos
posLenOfTok (TokMinus    pos  ) = pos
posLenOfTok (TokStar     pos  ) = pos
posLenOfTok (TokSlash    pos  ) = pos
posLenOfTok (TokPercent  pos  ) = pos
posLenOfTok (TokAmper    pos  ) = pos
posLenOfTok (TokShiftL   pos  ) = pos
posLenOfTok (TokShiftR   pos  ) = pos
posLenOfTok (TokLess     pos  ) = pos
posLenOfTok (TokLessEq   pos  ) = pos
posLenOfTok (TokHigh     pos  ) = pos
posLenOfTok (TokHighEq   pos  ) = pos
posLenOfTok (TokEqual    pos  ) = pos
posLenOfTok (TokUnequal  pos  ) = pos
posLenOfTok (TokHat      pos  ) = pos
posLenOfTok (TokBar      pos  ) = pos
posLenOfTok (TokAnd      pos  ) = pos
posLenOfTok (TokOr       pos  ) = pos
posLenOfTok (TokQuest    pos  ) = pos
posLenOfTok (TokColon    pos  ) = pos
posLenOfTok (TokAssign   pos  ) = pos
posLenOfTok (TokPlusAss  pos  ) = pos
posLenOfTok (TokMinusAss pos  ) = pos
posLenOfTok (TokStarAss  pos  ) = pos
posLenOfTok (TokSlashAss pos  ) = pos
posLenOfTok (TokPercAss  pos  ) = pos
posLenOfTok (TokAmpAss   pos  ) = pos
posLenOfTok (TokHatAss   pos  ) = pos
posLenOfTok (TokBarAss   pos  ) = pos
posLenOfTok (TokSLAss    pos  ) = pos
posLenOfTok (TokSRAss    pos  ) = pos
posLenOfTok (TokComma    pos  ) = pos
posLenOfTok (TokSemic    pos  ) = pos
posLenOfTok (TokLBrace   pos  ) = pos
posLenOfTok (TokRBrace   pos  ) = pos
posLenOfTok (TokEllipsis pos  ) = pos
posLenOfTok (TokAlignof  pos  ) = pos
posLenOfTok (TokAsm      pos  ) = pos
posLenOfTok (TokAuto     pos  ) = pos
posLenOfTok (TokBreak    pos  ) = pos
posLenOfTok (TokBool     pos  ) = pos
posLenOfTok (TokCase     pos  ) = pos
posLenOfTok (TokChar     pos  ) = pos
posLenOfTok (TokConst    pos  ) = pos
posLenOfTok (TokContinue pos  ) = pos
posLenOfTok (TokComplex  pos  ) = pos
posLenOfTok (TokDefault  pos  ) = pos
posLenOfTok (TokDo       pos  ) = pos
posLenOfTok (TokDouble   pos  ) = pos
posLenOfTok (TokElse     pos  ) = pos
posLenOfTok (TokEnum     pos  ) = pos
posLenOfTok (TokExtern   pos  ) = pos
posLenOfTok (TokFloat    pos  ) = pos
posLenOfTok (TokFor      pos  ) = pos
posLenOfTok (TokGoto     pos  ) = pos
posLenOfTok (TokInt      pos  ) = pos
posLenOfTok (TokInline   pos  ) = pos
posLenOfTok (TokIf       pos  ) = pos
posLenOfTok (TokLong     pos  ) = pos
posLenOfTok (TokLabel    pos  ) = pos
posLenOfTok (TokRegister pos  ) = pos
posLenOfTok (TokRestrict pos  ) = pos
posLenOfTok (TokReturn   pos  ) = pos
posLenOfTok (TokShort    pos  ) = pos
posLenOfTok (TokSigned   pos  ) = pos
posLenOfTok (TokSizeof   pos  ) = pos
posLenOfTok (TokStatic   pos  ) = pos
posLenOfTok (TokStruct   pos  ) = pos
posLenOfTok (TokSwitch   pos  ) = pos
posLenOfTok (TokTypedef  pos  ) = pos
posLenOfTok (TokTypeof   pos  ) = pos
posLenOfTok (TokThread   pos  ) = pos
posLenOfTok (TokUnion    pos  ) = pos
posLenOfTok (TokUnsigned pos  ) = pos
posLenOfTok (TokVoid     pos  ) = pos
posLenOfTok (TokVolatile pos  ) = pos
posLenOfTok (TokWhile    pos  ) = pos
posLenOfTok (TokCLit     pos _) = pos
posLenOfTok (TokILit     pos _) = pos
posLenOfTok (TokFLit     pos _) = pos
posLenOfTok (TokSLit     pos _) = pos
posLenOfTok (TokIdent    pos _) = pos
posLenOfTok (TokTyIdent  pos _) = pos
posLenOfTok (TokGnuC   _ pos  ) = pos
posLenOfTok TokEof = error "tokenPos: Eof"

instance Show Token where
  showsPrec _ (TokLParen   _  ) = showString "("
  showsPrec _ (TokRParen   _  ) = showString ")"
  showsPrec _ (TokLBracket _  ) = showString "["
  showsPrec _ (TokRBracket _  ) = showString "]"
  showsPrec _ (TokArrow    _  ) = showString "->"
  showsPrec _ (TokDot      _  ) = showString "."
  showsPrec _ (TokExclam   _  ) = showString "!"
  showsPrec _ (TokTilde    _  ) = showString "~"
  showsPrec _ (TokInc      _  ) = showString "++"
  showsPrec _ (TokDec      _  ) = showString "--"
  showsPrec _ (TokPlus     _  ) = showString "+"
  showsPrec _ (TokMinus    _  ) = showString "-"
  showsPrec _ (TokStar     _  ) = showString "*"
  showsPrec _ (TokSlash    _  ) = showString "/"
  showsPrec _ (TokPercent  _  ) = showString "%"
  showsPrec _ (TokAmper    _  ) = showString "&"
  showsPrec _ (TokShiftL   _  ) = showString "<<"
  showsPrec _ (TokShiftR   _  ) = showString ">>"
  showsPrec _ (TokLess     _  ) = showString "<"
  showsPrec _ (TokLessEq   _  ) = showString "<="
  showsPrec _ (TokHigh     _  ) = showString ">"
  showsPrec _ (TokHighEq   _  ) = showString ">="
  showsPrec _ (TokEqual    _  ) = showString "=="
  showsPrec _ (TokUnequal  _  ) = showString "!="
  showsPrec _ (TokHat      _  ) = showString "^"
  showsPrec _ (TokBar      _  ) = showString "|"
  showsPrec _ (TokAnd      _  ) = showString "&&"
  showsPrec _ (TokOr       _  ) = showString "||"
  showsPrec _ (TokQuest    _  ) = showString "?"
  showsPrec _ (TokColon    _  ) = showString ":"
  showsPrec _ (TokAssign   _  ) = showString "="
  showsPrec _ (TokPlusAss  _  ) = showString "+="
  showsPrec _ (TokMinusAss _  ) = showString "-="
  showsPrec _ (TokStarAss  _  ) = showString "*="
  showsPrec _ (TokSlashAss _  ) = showString "/="
  showsPrec _ (TokPercAss  _  ) = showString "%="
  showsPrec _ (TokAmpAss   _  ) = showString "&="
  showsPrec _ (TokHatAss   _  ) = showString "^="
  showsPrec _ (TokBarAss   _  ) = showString "|="
  showsPrec _ (TokSLAss    _  ) = showString "<<="
  showsPrec _ (TokSRAss    _  ) = showString ">>="
  showsPrec _ (TokComma    _  ) = showString ","
  showsPrec _ (TokSemic    _  ) = showString ";"
  showsPrec _ (TokLBrace   _  ) = showString "{"
  showsPrec _ (TokRBrace   _  ) = showString "}"
  showsPrec _ (TokEllipsis _  ) = showString "..."
  showsPrec _ (TokAlignof  _  ) = showString "alignof"
  showsPrec _ (TokAsm      _  ) = showString "asm"
  showsPrec _ (TokAuto     _  ) = showString "auto"
  showsPrec _ (TokBool _)       = showString "_Bool"
  showsPrec _ (TokBreak    _  ) = showString "break"
  showsPrec _ (TokCase     _  ) = showString "case"
  showsPrec _ (TokChar     _  ) = showString "char"
  showsPrec _ (TokComplex _)    = showString "_Complex"
  showsPrec _ (TokConst    _  ) = showString "const"
  showsPrec _ (TokContinue _  ) = showString "continue"
  showsPrec _ (TokDefault  _  ) = showString "default"
  showsPrec _ (TokDouble   _  ) = showString "double"
  showsPrec _ (TokDo       _  ) = showString "do"
  showsPrec _ (TokElse     _  ) = showString "else"
  showsPrec _ (TokEnum     _  ) = showString "enum"
  showsPrec _ (TokExtern   _  ) = showString "extern"
  showsPrec _ (TokFloat    _  ) = showString "float"
  showsPrec _ (TokFor      _  ) = showString "for"
  showsPrec _ (TokGoto     _  ) = showString "goto"
  showsPrec _ (TokIf       _  ) = showString "if"
  showsPrec _ (TokInline   _  ) = showString "inline"
  showsPrec _ (TokInt      _  ) = showString "int"
  showsPrec _ (TokLong     _  ) = showString "long"
  showsPrec _ (TokLabel    _  ) = showString "__label__"
  showsPrec _ (TokRegister _  ) = showString "register"
  showsPrec _ (TokRestrict _  ) = showString "restrict"
  showsPrec _ (TokReturn   _  ) = showString "return"
  showsPrec _ (TokShort    _  ) = showString "short"
  showsPrec _ (TokSigned   _  ) = showString "signed"
  showsPrec _ (TokSizeof   _  ) = showString "sizeof"
  showsPrec _ (TokStatic   _  ) = showString "static"
  showsPrec _ (TokStruct   _  ) = showString "struct"
  showsPrec _ (TokSwitch   _  ) = showString "switch"
  showsPrec _ (TokTypedef  _  ) = showString "typedef"
  showsPrec _ (TokTypeof   _  ) = showString "typeof"
  showsPrec _ (TokThread   _  ) = showString "__thread"
  showsPrec _ (TokUnion    _  ) = showString "union"
  showsPrec _ (TokUnsigned _  ) = showString "unsigned"
  showsPrec _ (TokVoid     _  ) = showString "void"
  showsPrec _ (TokVolatile _  ) = showString "volatile"
  showsPrec _ (TokWhile    _  ) = showString "while"
  showsPrec _ (TokCLit     _ c) = shows c
  showsPrec _ (TokILit     _ i) = shows i
  showsPrec _ (TokFLit     _ f) = shows f
  showsPrec _ (TokSLit     _ s) = shows s
  showsPrec _ (TokIdent    _ i) = (showString . identToString) i
  showsPrec _ (TokTyIdent  _ i) = (showString . identToString) i
  showsPrec _ (TokGnuC GnuCAttrTok _) = showString "__attribute__"
  showsPrec _ (TokGnuC GnuCExtTok  _) = showString "__extension__"
  showsPrec _ (TokGnuC GnuCComplexReal _) = showString "__real__"
  showsPrec _ (TokGnuC GnuCComplexImag  _) = showString "__imag__"
  showsPrec _ (TokGnuC GnuCVaArg    _) = showString "__builtin_va_arg"
  showsPrec _ (TokGnuC GnuCOffsetof _) = showString "__builtin_offsetof"
  showsPrec _ (TokGnuC GnuCTyCompat _) = showString "__builtin_types_compatible_p"
  showsPrec _ TokEof = error "show Token : TokEof"
