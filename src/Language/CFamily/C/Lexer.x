-----------------------------------------------------------------------------
-- Module      :  Lexer.x
-- Copyright   : (c) [1999..2004] Manuel M T Chakravarty
--               (c) 2005 Duncan Coutts
--               (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
--  Lexer for C files, after being processed by the C preprocessor
--
--  We assume that the input already went through cpp.  Thus, we do not handle
--  comments and preprocessor directives here.  It supports the
--  C99 `restrict' extension: <http://www.lysator.liu.se/c/restrict.html> as
--  well as inline functions.
--
--  Comments:
--
--  * Universal character names and multi-character character constants,
--    as well as trigraphs are unsupported. They are lexed, but yield an error.
--
--  * We add `typedef-name' (K&R 8.9) as a token, as proposed in K&R A13.
--    However, as these tokens cannot be recognized lexically, but require a
--    context analysis, they are never produced by the lexer, but instead have
--    to be introduced in a later phase (by converting the corresponding
--    identifiers).
--
--  * We also recognize GNU C `__attribute__', `__extension__', `__complex__',
--    `__const',  `__const__', `__imag', `__imag__', `__inline', `__inline__',
--    `int128', `__real', `__real__, `__restrict', and `__restrict__'.
--
--  * Any line starting with `#pragma' is ignored.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.
--
--  With C99 we refer to ``ISO/IEC 9899:TC3'',
--  available online at http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf.
--
--- TODO ----------------------------------------------------------------------
--
--  * There are more GNU C specific keywords.  Add them and change `Parser.y'
--    correspondingly (in particular, most tokens within __attribute ((...))
--    expressions are actually keywords, but we handle them as identifiers at
--    the moment).
--
--  * Add support for bytestrings

{
module Language.CFamily.C.Lexer where

import Language.CFamily.Constants
import Language.CFamily.Data
import Language.CFamily.Lexer
import Language.CFamily.Token

import Control.Monad (when)
}

$space = [ \ \t ]                           -- horizontal white space
$eol   = \n                                 -- end of line

$letter   = [a-zA-Z]
$identletter = [a-zA-Z_\$]                  -- GNU extension: allow $ in variable names
$octdigit = 0-7
$digit    = 0-9
$digitNZ  = 1-9
$hexdigit = [0-9a-fA-F]

$inchar   = . # [ \\ \' \n \r ]       -- valid character in char constant
$instr    = . # [ \\ \" \n \r ]       -- valid character in a string literal
$infname  = . # [ \\ \" ]             -- valid character in a filename

@sp  = $space*

-- character escape sequence (follows K&R A2.5.2)
--
-- * also used for strings
-- * C99: 6.4.4.4
@charesc  = \\([ntvbrfaeE\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)
@ucn      = \\u$hexdigit{4}|\\U$hexdigit{8}

-- components of integer constants
--
-- * C99: 6.4.4.1
@int = $digitNZ$digit*

-- integer suffixes
@llsuffix  = ll|LL
@gnusuffix = [ij]?
@intsuffix = [uU][lL]?|[uU]@llsuffix|[lL][uU]?|@llsuffix[uU]?
@intgnusuffix = @intsuffix@gnusuffix?|@gnusuffix@intsuffix?

-- components of float constants (follows K&R A2.5.3)
--
-- * C99: 6.4.4.2
@digits    = $digit+
@intpart   = @digits
@fractpart = @digits

@mantpart  = @intpart?\.@fractpart|@intpart\.
@exppart   = [eE][\+\-]?@digits

@hexprefix = 0x
@hexdigits = $hexdigit+
@hexmant   = @hexdigits?\.@hexdigits|@hexdigits\.
@binexp    = [pP][\+\-]?@digits

@floatsuffix    = [fFlL]
@floatgnusuffix = @floatsuffix@gnusuffix?|@gnusuffix@floatsuffix?



tokens :-

-- whitespace (follows K&R A2.1)
--
-- * horizontal and vertical tabs, newlines, and form feeds are filter out by
--   `Lexers.ctrlLexer'
--
-- * comments are not handled, as we assume the input already went through cpp
--
$white+         ;

-- #line directive (K&R A12.6)
--
-- * allows further ints after the file name a la GCC; as the GCC CPP docu
--   doesn't say how many ints there can be, we allow an unbound number
--
\#$space*@int$space*(\"($infname|@charesc)*\"$space*)?(@int$space*)*\r?$eol
  { \pos len str -> setPos (adjustLineDirective len (takeChars len str) pos) >> lexToken' False }

-- #pragma directive (K&R A12.8)
--
-- * we simply ignore any #pragma (but take care to update the position
--   information)
--
\#$space*pragma.*$eol   ;

-- #ident directive, eg used by rcs/cvs
--
-- * we simply ignore any #ident (but take care to update the position
--   information)
--
\#$space*ident.*$eol    ;

-- identifiers and keywords (follows K&R A2.3 and A2.4)
--
$identletter($identletter|$digit)*  { \pos len str -> idkwtok (takeChars len str) pos }

-- constants (follows K&R A2.5)
--
-- * K&R,C99 explicitly mention `enumeration-constants'; however, as they are
--   lexically identifiers, we do not have an extra case for them
--

-- integer constants (follows K&R A2.5.1, C99 6.4.4.1)
-- NOTE: 0 is lexed as octal integer constant, and readCOctal takes care of this
0$octdigit*@intgnusuffix?       { token_plus TokILit readCOctal }
$digitNZ$digit*@intgnusuffix?   { token_plus TokILit (readCInteger DecRepr) }
0[xX]$hexdigit+@intgnusuffix?   { token_plus TokILit (readCInteger HexRepr . drop 2) }

(0$octdigit*|$digitNZ$digit*|0[xX]$hexdigit+)[uUlL]+ { token_fail "Invalid integer constant suffix" }

-- character constants (follows K&R A2.5.2, C99 6.4.4.4)
--
-- * Universal Character Names are unsupported and cause an error.
\'($inchar|@charesc)\'  { token TokCLit (cChar . fst . unescapeChar . tail) }
L\'($inchar|@charesc)\' { token TokCLit (cChar_w . fst . unescapeChar . tail . tail) }
\'($inchar|@charesc){2,}\' { token TokCLit (flip cChars False . unescapeMultiChars .tail) }
L\'($inchar|@charesc){2,}\' { token TokCLit (flip cChars True . unescapeMultiChars . tail . tail) }

-- float constants (follows K&R A2.5.3. C99 6.4.4.2)
--
-- * NOTE: Hexadecimal floating constants without binary exponents are forbidden.
--         They generate a lexer error, because they are hard to recognize in the parser.
(@mantpart@exppart?|@intpart@exppart)@floatgnusuffix?  { token TokFLit readCFloat }
@hexprefix(@hexmant|@hexdigits)@binexp@floatgnusuffix? { token TokFLit readCFloat }
@hexprefix@hexmant                                     { token_fail "Hexadecimal floating constant requires an exponent" }

-- string literal (follows K&R A2.6)
-- C99: 6.4.5.
\"($instr|@charesc)*\"      { token TokSLit (cString . unescapeString . init . tail) }
L\"($instr|@charesc)*\"     { token TokSLit (cString_w . unescapeString . init . tail . tail) }

L?\'@ucn\'                        { token_fail "Universal character names are unsupported" }
L?\'\\[^0-7'\"\?\\abfnrtvuUx]\'     { token_fail "Invalid escape sequence" }
L?\"($inchar|@charesc)*@ucn($inchar|@charesc|@ucn)*\" { token_fail "Universal character names in string literals are unsupported"}

-- operators and separators
--
"{"      { token_ 1 TokBraceL              }
"}"      { token_ 1 TokBraceR              }
"["      { token_ 1 TokBracketL            }
"]"      { token_ 1 TokBracketR            }
"("      { token_ 1 TokParenL              }
")"      { token_ 1 TokParenR              }
"<:"     { token_ 2 TokBracketL            }
":>"     { token_ 2 TokBracketR            }
"<%"     { token_ 2 TokBraceL              }
"%>"     { token_ 2 TokBraceR              }
";"      { token_ 1 TokSemicolon           }
":"      { token_ 1 TokColon               }
"..."    { token_ 3 TokEllipsis            }
"?"      { token_ 1 TokQuestion            }
"::"     { token_ 2 TokColonColon          }
"."      { token_ 1 TokDot                 }
".*"     { token_ 2 TokDotStar             }
"+"      { token_ 1 TokPlus                }
"-"      { token_ 1 TokMinus               }
"*"      { token_ 1 TokStar                }
"/"      { token_ 1 TokSlash               }
"%"      { token_ 1 TokPercent             }
"^"      { token_ 1 TokHat                 }
"&"      { token_ 1 TokAmpersand           }
"|"      { token_ 1 TokBar                 }
"~"      { token_ 1 TokTilde               }
"="      { token_ 1 TokEqual               }
"!"      { token_ 1 TokExclamation         }
"<"      { token_ 1 TokLess                }
">"      { token_ 1 TokGreater             }
"+="     { token_ 2 TokPlusEqual           }
"-="     { token_ 2 TokMinusEqual          }
"*="     { token_ 2 TokStarEqual           }
"/="     { token_ 2 TokSlashEqual          }
"%="     { token_ 2 TokPercentEqual        }
"^="     { token_ 2 TokHatEqual            }
"&="     { token_ 2 TokAmpersandEqual      }
"|="     { token_ 2 TokBarEqual            }
"<<"     { token_ 2 TokLessLess            }
">>"     { token_ 2 TokGreaterGreater      }
">>="    { token_ 3 TokGreaterGreaterEqual }
"<<="    { token_ 3 TokLessLessEqual       }
"=="     { token_ 2 TokEqualEqual          }
"!="     { token_ 2 TokExclamationEqual    }
"<="     { token_ 2 TokLessEqual           }
">="     { token_ 2 TokGreaterEqual        }
"&&"     { token_ 2 TokAmpersandAmpersand  }
"||"     { token_ 2 TokBarBar              }
"++"     { token_ 2 TokPlusPlus            }
"--"     { token_ 2 TokMinusMinus          }
","      { token_ 1 TokComma               }
"->"     { token_ 2 TokHyphenGreater       }


{
-- We use the odd looking list of string patterns here rather than normal
-- string literals since GHC converts the latter into a sequence of string
-- comparisons (ie a linear search) but it translates the former using its
-- effecient pattern matching which gives us the expected radix-style search.
-- This change makes a significant performance difference [chak]
--
-- To make this a little more maintainable, we autogenerate it from this list,
-- using the script GenerateKeywordMatch.hs (in /src)
-- Tokens: alignof __alignof __alignof__ asm __asm __asm__ __attribute __attribute__ auto _Bool break __builtin_offsetof __builtin_types_compatible_p __builtin_va_arg case char _Complex __complex__ const __const __const__ continue default do double else enum __extension__ extern float for goto if __imag __imag__ inline __inline __inline__ int __int128 __label__ long __real __real__ register __restrict __restrict__ return short signed __signed __signed__ sizeof static struct switch __thread typedef typeof __typeof __typeof__ union unsigned void volatile __volatile __volatile__ while
idkwtok ('_' : 'B' : 'o' : 'o' : 'l' : []) = tok 5 TokBool
idkwtok ('_' : 'C' : 'o' : 'm' : 'p' : 'l' : 'e' : 'x' : []) = tok 8 TokComplex
idkwtok ('_' : '_' : 'a' : 'l' : 'i' : 'g' : 'n' : 'o' : 'f' : []) = tok 9 TokAlignof
idkwtok ('a' : 'l' : 'i' : 'g' : 'n' : 'o' : 'f' : []) = tok 7 TokAlignof
idkwtok ('_' : '_' : 'a' : 'l' : 'i' : 'g' : 'n' : 'o' : 'f' : '_' : '_' : []) = tok 11 TokAlignof
idkwtok ('_' : '_' : 'a' : 's' : 'm' : []) = tok 5 TokAsm
idkwtok ('a' : 's' : 'm' : []) = tok 3 TokAsm
idkwtok ('_' : '_' : 'a' : 's' : 'm' : '_' : '_' : []) = tok 7 TokAsm
idkwtok ('_' : '_' : 'a' : 't' : 't' : 'r' : 'i' : 'b' : 'u' : 't' : 'e' : []) = tok 11 (TokGnuC GnuCAttrTok)
idkwtok ('_' : '_' : 'a' : 't' : 't' : 'r' : 'i' : 'b' : 'u' : 't' : 'e' : '_' : '_' : []) = tok 13 (TokGnuC GnuCAttrTok)
idkwtok ('a' : 'u' : 't' : 'o' : []) = tok 4 TokAuto
idkwtok ('b' : 'r' : 'e' : 'a' : 'k' : []) = tok 5 TokBreak
idkwtok ('_' : '_' : 'b' : 'u' : 'i' : 'l' : 't' : 'i' : 'n' : '_' : 'o' : 'f' : 'f' : 's' : 'e' : 't' : 'o' : 'f' : []) = tok 18 (TokGnuC GnuCOffsetof)
idkwtok ('_' : '_' : 'b' : 'u' : 'i' : 'l' : 't' : 'i' : 'n' : '_' : 't' : 'y' : 'p' : 'e' : 's' : '_' : 'c' : 'o' : 'm' : 'p' : 'a' : 't' : 'i' : 'b' : 'l' : 'e' : '_' : 'p' : []) = tok 28 (TokGnuC GnuCTyCompat)
idkwtok ('_' : '_' : 'b' : 'u' : 'i' : 'l' : 't' : 'i' : 'n' : '_' : 'v' : 'a' : '_' : 'a' : 'r' : 'g' : []) = tok 16 (TokGnuC GnuCVaArg)
idkwtok ('c' : 'a' : 's' : 'e' : []) = tok 4 TokCase
idkwtok ('c' : 'h' : 'a' : 'r' : []) = tok 4 TokChar
idkwtok ('_' : '_' : 'c' : 'o' : 'm' : 'p' : 'l' : 'e' : 'x' : '_' : '_' : []) = tok 11 TokComplex
idkwtok ('_' : '_' : 'c' : 'o' : 'n' : 's' : 't' : []) = tok 7 TokConst
idkwtok ('c' : 'o' : 'n' : 's' : 't' : []) = tok 5 TokConst
idkwtok ('_' : '_' : 'c' : 'o' : 'n' : 's' : 't' : '_' : '_' : []) = tok 9 TokConst
idkwtok ('c' : 'o' : 'n' : 't' : 'i' : 'n' : 'u' : 'e' : []) = tok 8 TokContinue
idkwtok ('d' : 'e' : 'f' : 'a' : 'u' : 'l' : 't' : []) = tok 7 TokDefault
idkwtok ('d' : 'o' : []) = tok 2 TokDo
idkwtok ('d' : 'o' : 'u' : 'b' : 'l' : 'e' : []) = tok 6 TokDouble
idkwtok ('e' : 'l' : 's' : 'e' : []) = tok 4 TokElse
idkwtok ('e' : 'n' : 'u' : 'm' : []) = tok 4 TokEnum
idkwtok ('_' : '_' : 'e' : 'x' : 't' : 'e' : 'n' : 's' : 'i' : 'o' : 'n' : '_' : '_' : []) = tok 13 (TokGnuC GnuCExtTok)
idkwtok ('e' : 'x' : 't' : 'e' : 'r' : 'n' : []) = tok 6 TokExtern
idkwtok ('f' : 'l' : 'o' : 'a' : 't' : []) = tok 5 TokFloat
idkwtok ('f' : 'o' : 'r' : []) = tok 3 TokFor
idkwtok ('g' : 'o' : 't' : 'o' : []) = tok 4 TokGoto
idkwtok ('i' : 'f' : []) = tok 2 TokIf
idkwtok ('_' : '_' : 'i' : 'm' : 'a' : 'g' : []) = tok 6 (TokGnuC GnuCComplexImag)
idkwtok ('_' : '_' : 'i' : 'm' : 'a' : 'g' : '_' : '_' : []) = tok 8 (TokGnuC GnuCComplexImag)
idkwtok ('_' : '_' : 'i' : 'n' : 'l' : 'i' : 'n' : 'e' : []) = tok 8 TokInline
idkwtok ('_' : '_' : 'i' : 'n' : 't' : '1' : '2' : '8' : []) = tok 8 (TokGnuC GnuCInt128)
idkwtok ('i' : 'n' : 'l' : 'i' : 'n' : 'e' : []) = tok 6 TokInline
idkwtok ('_' : '_' : 'i' : 'n' : 'l' : 'i' : 'n' : 'e' : '_' : '_' : []) = tok 10 TokInline
idkwtok ('i' : 'n' : 't' : []) = tok 3 TokInt
idkwtok ('_' : '_' : 'l' : 'a' : 'b' : 'e' : 'l' : '_' : '_' : []) = tok 9 TokLabel
idkwtok ('l' : 'o' : 'n' : 'g' : []) = tok 4 TokLong
idkwtok ('_' : '_' : 'r' : 'e' : 'a' : 'l' : []) = tok 6 (TokGnuC GnuCComplexReal)
idkwtok ('_' : '_' : 'r' : 'e' : 'a' : 'l' : '_' : '_' : []) = tok 8 (TokGnuC GnuCComplexReal)
idkwtok ('r' : 'e' : 'g' : 'i' : 's' : 't' : 'e' : 'r' : []) = tok 8 TokRegister
idkwtok ('_' : '_' : 'r' : 'e' : 's' : 't' : 'r' : 'i' : 'c' : 't' : []) = tok 10 TokRestrict
idkwtok ('r' : 'e' : 's' : 't' : 'r' : 'i' : 'c' : 't' : []) = tok 8 TokRestrict
idkwtok ('_' : '_' : 'r' : 'e' : 's' : 't' : 'r' : 'i' : 'c' : 't' : '_' : '_' : []) = tok 12 TokRestrict
idkwtok ('r' : 'e' : 't' : 'u' : 'r' : 'n' : []) = tok 6 TokReturn
idkwtok ('s' : 'h' : 'o' : 'r' : 't' : []) = tok 5 TokShort
idkwtok ('_' : '_' : 's' : 'i' : 'g' : 'n' : 'e' : 'd' : []) = tok 8 TokSigned
idkwtok ('s' : 'i' : 'g' : 'n' : 'e' : 'd' : []) = tok 6 TokSigned
idkwtok ('_' : '_' : 's' : 'i' : 'g' : 'n' : 'e' : 'd' : '_' : '_' : []) = tok 10 TokSigned
idkwtok ('s' : 'i' : 'z' : 'e' : 'o' : 'f' : []) = tok 6 TokSizeof
idkwtok ('s' : 't' : 'a' : 't' : 'i' : 'c' : []) = tok 6 TokStatic
idkwtok ('s' : 't' : 'r' : 'u' : 'c' : 't' : []) = tok 6 TokStruct
idkwtok ('s' : 'w' : 'i' : 't' : 'c' : 'h' : []) = tok 6 TokSwitch
idkwtok ('_' : '_' : 't' : 'h' : 'r' : 'e' : 'a' : 'd' : []) = tok 8 TokThread
idkwtok ('t' : 'y' : 'p' : 'e' : 'd' : 'e' : 'f' : []) = tok 7 TokTypedef
idkwtok ('_' : '_' : 't' : 'y' : 'p' : 'e' : 'o' : 'f' : []) = tok 8 TokTypeof
idkwtok ('t' : 'y' : 'p' : 'e' : 'o' : 'f' : []) = tok 6 TokTypeof
idkwtok ('_' : '_' : 't' : 'y' : 'p' : 'e' : 'o' : 'f' : '_' : '_' : []) = tok 10 TokTypeof
idkwtok ('u' : 'n' : 'i' : 'o' : 'n' : []) = tok 5 TokUnion
idkwtok ('u' : 'n' : 's' : 'i' : 'g' : 'n' : 'e' : 'd' : []) = tok 8 TokUnsigned
idkwtok ('v' : 'o' : 'i' : 'd' : []) = tok 4 TokVoid
idkwtok ('_' : '_' : 'v' : 'o' : 'l' : 'a' : 't' : 'i' : 'l' : 'e' : []) = tok 10 TokVolatile
idkwtok ('v' : 'o' : 'l' : 'a' : 't' : 'i' : 'l' : 'e' : []) = tok 8 TokVolatile
idkwtok ('_' : '_' : 'v' : 'o' : 'l' : 'a' : 't' : 'i' : 'l' : 'e' : '_' : '_' : []) = tok 12 TokVolatile
idkwtok ('w' : 'h' : 'i' : 'l' : 'e' : []) = tok 5 TokWhile

#include "src/Language/CFamily/Lexer.hs-inc"
}

