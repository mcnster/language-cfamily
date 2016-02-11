-- ---------------------------------------------------------------------------
--
-- Module      :  Language.CFamily.Token
-- Copyright   :  [1999..2004] Manuel M T Chakravarty
--                2005 Duncan Coutts
--                2016 Mick Nelso
-- License     :  BSD3
-- Maintainer  :  micknelso@gmail.com
-- Portability :  portable
--
-- Tokens for the C and C++ lexer/parsers.
--
-- ---------------------------------------------------------------------------

module Language.CFamily.Token where

import Language.CFamily.Constants

import Language.CFamily.Data.Ident
import Language.CFamily.Data.Position


data Token = TokParenL              !PosLength            -- `('
           | TokParenR              !PosLength            -- `)'
           | TokBracketL            !PosLength            -- `['
           | TokBracketR            !PosLength            -- `]'
           | TokBarBar              !PosLength            -- `||'
           | TokDotStar             !PosLength            -- `.*'
           | TokColonColon          !PosLength            -- `::'
           | TokHyphenGreaterStar   !PosLength            -- `->*'
           | TokHyphenGreater       !PosLength            -- `->'
           | TokDot                 !PosLength            -- `.'
           | TokExclamation         !PosLength            -- `!'
           | TokTilde               !PosLength            -- `~'
           | TokPlusPlus            !PosLength            -- `++'
           | TokMinusMinus          !PosLength            -- `--'
           | TokPlus                !PosLength            -- `+'
           | TokMinus               !PosLength            -- `-'
           | TokStar                !PosLength            -- `*'
           | TokSlash               !PosLength            -- `/'
           | TokPercent             !PosLength            -- `%'
           | TokAmpersand           !PosLength            -- `&'
           | TokLessLess            !PosLength            -- `<<'
           | TokGreaterGreater      !PosLength            -- `>>'
           | TokLess                !PosLength            -- `<'
           | TokLessEqual           !PosLength            -- `<='
           | TokGreater             !PosLength            -- `>'
           | TokGreaterEqual        !PosLength            -- `>='
           | TokEqualEqual          !PosLength            -- `=='
           | TokExclamationEqual    !PosLength            -- `!='
           | TokHat                 !PosLength            -- `^'
           | TokBar                 !PosLength            -- `|'
           | TokAmpersandAmpersand  !PosLength            -- `&&'
           | TokQuestion            !PosLength            -- `?'
           | TokColon               !PosLength            -- `:'
           | TokEqual               !PosLength            -- `='
           | TokPlusEqual           !PosLength            -- `+='
           | TokMinusEqual          !PosLength            -- `-='
           | TokStarEqual           !PosLength            -- `*='
           | TokSlashEqual          !PosLength            -- `/='
           | TokPercentEqual        !PosLength            -- `%='
           | TokAmpersandEqual      !PosLength            -- `&='
           | TokHatEqual            !PosLength            -- `^='
           | TokBarEqual            !PosLength            -- `|='
           | TokLessLessEqual       !PosLength            -- `<<='
           | TokGreaterGreaterEqual !PosLength            -- `>>='
           | TokComma               !PosLength            -- `,'
           | TokSemicolon           !PosLength            -- `;'
           | TokBraceL              !PosLength            -- `{'
           | TokBraceR              !PosLength            -- `}'
           | TokEllipsis            !PosLength            -- `...'
           | TokAlignas             !PosLength             -- `alignas'
           | TokAlignof             !PosLength             -- `alignof', `__alignof', `__alignof__'
           | TokAsm                 !PosLength             -- `asm', `__asm', `__asm__'
           | TokAuto                !PosLength             -- `auto'
           | TokBreak               !PosLength             -- `break'
           | TokBool                !PosLength             -- `_Bool'
           | TokCase                !PosLength             -- `case'
           | TokCatch               !PosLength             -- `catch'
           | TokChar                !PosLength             -- `char'
           | TokChar16              !PosLength             -- `char16_t'
           | TokChar32              !PosLength             -- `char32_t'
           | TokClass               !PosLength             -- `class'
           | TokConst               !PosLength             -- `const', `__const', `__const__'
           | TokConstExpr           !PosLength             -- `constexpr'
           | TokConstCast           !PosLength             -- `const_cast'
           | TokContinue            !PosLength             -- `continue'
           | TokComplex             !PosLength             -- `_Complex'
           | TokDeclType            !PosLength             -- `decltype'
           | TokDefault             !PosLength             -- `default'
           | TokDelete              !PosLength             -- `delete'
           | TokDo                  !PosLength             -- `do'
           | TokDouble              !PosLength             -- `double'
           | TokDynCast             !PosLength             -- `dynamic_cast'
           | TokElse                !PosLength             -- `else'
           | TokEnum                !PosLength             -- `enum'
           | TokExplicit            !PosLength             -- `explicit'
           | TokExport              !PosLength             -- `export'
           | TokExtern              !PosLength             -- `extern'
           | TokFalse               !PosLength             -- `false'
           | TokFloat               !PosLength             -- `float'
           | TokFor                 !PosLength             -- `for'
           | TokFriend              !PosLength             -- `friend'
           | TokGoto                !PosLength             -- `goto'
           | TokIf                  !PosLength             -- `if'
           | TokInline              !PosLength             -- `inline', `__inline', `__inline__'
           | TokInt                 !PosLength             -- `int'
           | TokLong                !PosLength             -- `long'
           | TokLabel               !PosLength             -- `__label__'
           | TokMutable             !PosLength             -- `mutable'
           | TokNamespace           !PosLength             -- `namespace'
           | TokNew                 !PosLength             -- `new'
           | TokNoExcept            !PosLength             -- `noexcept'
           | TokOperator            !PosLength             -- `operator'
           | TokPrivate             !PosLength             -- `private'
           | TokProtected           !PosLength             -- `protected'
           | TokPublic              !PosLength             -- `public'
           | TokRegister            !PosLength             -- `register'
           | TokReinterpretCast     !PosLength             -- `reinterpret_cast'
           | TokRestrict            !PosLength             -- `restrict', `__restrict', `__restrict__'
           | TokReturn              !PosLength             -- `return'
           | TokShort               !PosLength             -- `short'
           | TokSigned              !PosLength             -- `signed', `__signed', `__signed__'
           | TokSizeof              !PosLength             -- `sizeof'
           | TokStatic              !PosLength              -- `static'
           | TokStaticAssert        !PosLength              -- `static_assert'
           | TokStaticCast          !PosLength              -- `static_cast'
           | TokStruct              !PosLength              -- `struct'
           | TokSwitch              !PosLength              -- `switch'
           | TokTemplate            !PosLength              -- `template'
           | TokThis                !PosLength              -- `this'
           | TokThreadLocal         !PosLength              -- `thread_local'
           | TokThrow               !PosLength              -- `throw'
           | TokTrue                !PosLength              -- `true'
           | TokTypedef             !PosLength              -- `typedef'
           | TokTypeid              !PosLength              -- `typeid'
           | TokTypename            !PosLength              -- `typename'
           | TokTypeof              !PosLength              -- `typeof'
           | TokThread              !PosLength              -- `__thread'
           | TokUnion               !PosLength              -- `union'
           | TokUnsigned            !PosLength              -- `unsigned'
           | TokUsing               !PosLength              -- `using'
           | TokVirtual             !PosLength              -- `virtual'
           | TokVoid                !PosLength              -- `void'
           | TokVolatile            !PosLength              -- `volatile', `__volatile', `__volatile__'
           | TokWChar               !PosLength              -- `wchar_t'
           | TokWhile               !PosLength              -- `while'
           | TokLitChar             !PosLength  !LitChar    -- character constant
           | TokLitInteger          !PosLength  !LitInteger -- integer constant
           | TokLitFloat            !PosLength  !LitFloat   -- float constant
           | TokLitString           !PosLength  LitString   -- string constant
           | TokLitPtr              !PosLength              -- `nullptr'
           | TokLitUserDef          !PosLength  LitUserDef
           | TokIdent               !PosLength  !Ident      -- identifier
           | TokTyIdent             !PosLength  !Ident      -- `typedef-name' identifier
           | TokGnuC     !GnuTok    !PosLength              -- special GNU tokens
           | TokEof                                         -- end of file
     deriving (Show)

-- Special tokens used in GNU C extensions to ANSI C.
--
data GnuTok = GnuCAttrTok              -- `__attribute__'
            | GnuCExtTok               -- `__extension__'
            | GnuCVaArg                -- `__builtin_va_arg'
            | GnuCOffsetof             -- `__builtin_offsetof'
            | GnuCTyCompat             -- `__builtin_types_compatible_p'
            | GnuCComplexReal          -- `__real__'
            | GnuCComplexImag          -- `__imag__'
            | GnuCInt128               -- `__int128
     deriving (Show)

instance Pos Token where
   posOf = fst . posLenOfTok

-- token position and length
--
-- MCNFIXME: C++ not implemented
posLenOfTok
   :: Token
   -> (Position, Int)
posLenOfTok (TokParenL              pos  ) = pos
posLenOfTok (TokParenR              pos  ) = pos
posLenOfTok (TokBracketL            pos  ) = pos
posLenOfTok (TokBracketR            pos  ) = pos
posLenOfTok (TokBarBar              pos  ) = pos
posLenOfTok (TokDotStar             pos  ) = pos
posLenOfTok (TokColonColon          pos  ) = pos
posLenOfTok (TokHyphenGreaterStar   pos  ) = pos
posLenOfTok (TokHyphenGreater       pos  ) = pos
posLenOfTok (TokDot                 pos  ) = pos
posLenOfTok (TokExclamation         pos  ) = pos
posLenOfTok (TokTilde               pos  ) = pos
posLenOfTok (TokPlusPlus            pos  ) = pos
posLenOfTok (TokMinusMinus          pos  ) = pos
posLenOfTok (TokPlus                pos  ) = pos
posLenOfTok (TokMinus               pos  ) = pos
posLenOfTok (TokStar                pos  ) = pos
posLenOfTok (TokSlash               pos  ) = pos
posLenOfTok (TokPercent             pos  ) = pos
posLenOfTok (TokAmpersand           pos  ) = pos
posLenOfTok (TokLessLess            pos  ) = pos
posLenOfTok (TokGreaterGreater      pos  ) = pos
posLenOfTok (TokLess                pos  ) = pos
posLenOfTok (TokLessEqual           pos  ) = pos
posLenOfTok (TokGreater             pos  ) = pos
posLenOfTok (TokGreaterEqual        pos  ) = pos
posLenOfTok (TokEqualEqual          pos  ) = pos
posLenOfTok (TokExclamationEqual    pos  ) = pos
posLenOfTok (TokHat                 pos  ) = pos
posLenOfTok (TokBar                 pos  ) = pos
posLenOfTok (TokAmpersandAmpersand  pos  ) = pos
posLenOfTok (TokQuestion            pos  ) = pos
posLenOfTok (TokColon               pos  ) = pos
posLenOfTok (TokEqual               pos  ) = pos
posLenOfTok (TokPlusEqual           pos  ) = pos
posLenOfTok (TokMinusEqual          pos  ) = pos
posLenOfTok (TokStarEqual           pos  ) = pos
posLenOfTok (TokSlashEqual          pos  ) = pos
posLenOfTok (TokPercentEqual        pos  ) = pos
posLenOfTok (TokAmpersandEqual      pos  ) = pos
posLenOfTok (TokHatEqual            pos  ) = pos
posLenOfTok (TokBarEqual            pos  ) = pos
posLenOfTok (TokLessLessEqual       pos  ) = pos
posLenOfTok (TokGreaterGreaterEqual pos  ) = pos
posLenOfTok (TokComma               pos  ) = pos
posLenOfTok (TokSemicolon           pos  ) = pos
posLenOfTok (TokBraceL              pos  ) = pos
posLenOfTok (TokBraceR              pos  ) = pos
posLenOfTok (TokEllipsis            pos  ) = pos
posLenOfTok (TokAlignas             pos  ) = pos
posLenOfTok (TokAlignof             pos  ) = pos
posLenOfTok (TokAsm                 pos  ) = pos
posLenOfTok (TokAuto                pos  ) = pos
posLenOfTok (TokBreak               pos  ) = pos
posLenOfTok (TokBool                pos  ) = pos
posLenOfTok (TokCase                pos  ) = pos
posLenOfTok (TokCatch               pos  ) = pos
posLenOfTok (TokChar                pos  ) = pos
posLenOfTok (TokChar16              pos  ) = pos
posLenOfTok (TokChar32              pos  ) = pos
posLenOfTok (TokClass               pos  ) = pos
posLenOfTok (TokConst               pos  ) = pos
posLenOfTok (TokConstExpr           pos  ) = pos
posLenOfTok (TokConstCast           pos  ) = pos
posLenOfTok (TokContinue            pos  ) = pos
posLenOfTok (TokComplex             pos  ) = pos
posLenOfTok (TokDeclType            pos  ) = pos
posLenOfTok (TokDefault             pos  ) = pos
posLenOfTok (TokDelete              pos  ) = pos
posLenOfTok (TokDo                  pos  ) = pos
posLenOfTok (TokDouble              pos  ) = pos
posLenOfTok (TokDynCast             pos  ) = pos
posLenOfTok (TokElse                pos  ) = pos
posLenOfTok (TokEnum                pos  ) = pos
posLenOfTok (TokExplicit            pos  ) = pos
posLenOfTok (TokExport              pos  ) = pos
posLenOfTok (TokExtern              pos  ) = pos
posLenOfTok (TokFalse               pos  ) = pos
posLenOfTok (TokFloat               pos  ) = pos
posLenOfTok (TokFor                 pos  ) = pos
posLenOfTok (TokFriend              pos  ) = pos
posLenOfTok (TokGoto                pos  ) = pos
posLenOfTok (TokIf                  pos  ) = pos
posLenOfTok (TokInline              pos  ) = pos
posLenOfTok (TokInt                 pos  ) = pos
posLenOfTok (TokLong                pos  ) = pos
posLenOfTok (TokLabel               pos  ) = pos
posLenOfTok (TokMutable             pos  ) = pos
posLenOfTok (TokNamespace           pos  ) = pos
posLenOfTok (TokNew                 pos  ) = pos
posLenOfTok (TokNoExcept            pos  ) = pos
posLenOfTok (TokOperator            pos  ) = pos
posLenOfTok (TokPrivate             pos  ) = pos
posLenOfTok (TokProtected           pos  ) = pos
posLenOfTok (TokPublic              pos  ) = pos
posLenOfTok (TokRegister            pos  ) = pos
posLenOfTok (TokReinterpretCast     pos  ) = pos
posLenOfTok (TokRestrict            pos  ) = pos
posLenOfTok (TokReturn              pos  ) = pos
posLenOfTok (TokShort               pos  ) = pos
posLenOfTok (TokSigned              pos  ) = pos
posLenOfTok (TokSizeof              pos  ) = pos
posLenOfTok (TokStatic              pos  ) = pos
posLenOfTok (TokStaticAssert        pos  ) = pos
posLenOfTok (TokStaticCast          pos  ) = pos
posLenOfTok (TokStruct              pos  ) = pos
posLenOfTok (TokSwitch              pos  ) = pos
posLenOfTok (TokTemplate            pos  ) = pos
posLenOfTok (TokThis                pos  ) = pos
posLenOfTok (TokThreadLocal         pos  ) = pos
posLenOfTok (TokThrow               pos  ) = pos
posLenOfTok (TokTrue                pos  ) = pos
posLenOfTok (TokTypedef             pos  ) = pos
posLenOfTok (TokTypeid              pos  ) = pos
posLenOfTok (TokTypename            pos  ) = pos
posLenOfTok (TokTypeof              pos  ) = pos
posLenOfTok (TokThread              pos  ) = pos
posLenOfTok (TokUnion               pos  ) = pos
posLenOfTok (TokUnsigned            pos  ) = pos
posLenOfTok (TokUsing               pos  ) = pos
posLenOfTok (TokVirtual             pos  ) = pos
posLenOfTok (TokVoid                pos  ) = pos
posLenOfTok (TokVolatile            pos  ) = pos
posLenOfTok (TokWChar               pos  ) = pos
posLenOfTok (TokWhile               pos  ) = pos
posLenOfTok (TokLitChar             pos _) = pos
posLenOfTok (TokLitInteger          pos _) = pos
posLenOfTok (TokLitFloat            pos _) = pos
posLenOfTok (TokLitString           pos _) = pos
posLenOfTok (TokLitPtr              pos  ) = pos
posLenOfTok (TokLitUserDef          pos _) = pos
posLenOfTok (TokIdent               pos _) = pos
posLenOfTok (TokTyIdent             pos _) = pos
posLenOfTok (TokGnuC              _ pos  ) = pos
posLenOfTok (TokEof                      ) = error "Token.posLenOfTok: TokEof"

{-
-- MCNFIXME: C++ not implemented
instance Show Token where
  showsPrec _ (TokParenL   _  ) = showString "("
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
  showsPrec _ (TokOr       _  ) = showString "|"
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
  showsPrec _ (TokGnuC GnuCInt128 _) = showString "__int128"
  showsPrec _ (TokGnuC GnuCVaArg    _) = showString "__builtin_va_arg"
  showsPrec _ (TokGnuC GnuCOffsetof _) = showString "__builtin_offsetof"
  showsPrec _ (TokGnuC GnuCTyCompat _) = showString "__builtin_types_compatible_p"
  showsPrec _ TokEof = error "show Token : TokEof"
-}
