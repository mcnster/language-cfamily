-- ----------------------------------------------------------------------------
-- 
-- Module      : Language.CFamily.Constants
-- Copyright   : (c) 2007..2008 Duncan Coutts, Benedikt Huber
--               (c) 2016 Mick Nelso
-- License     : BSD3
-- Maintainer  : micknelso@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- This module provides support for representing, checking and exporting c
-- constants, i.e. integral, float, character and string constants.
-- ----------------------------------------------------------------------------

module Language.CFamily.Constants where

import Language.CFamily.Data.Ident
import Language.CFamily.Data.Name
import Language.CFamily.Data.Position

import Data.Char
import Data.List
import Data.Maybe

import Numeric

-- ----------------------------------------------------------------------------

data LitInteger = LitInteger {
                          litIntValue  :: !Integer
                        , litIntRadix  :: !Int
                        , litIntType   :: !LitIntType
                  }
     deriving (Show)

data LitIntType = LitIntType {
                          litIntTypeUnsigned :: !Bool
                        , litIntTypeLong     :: !LitIntLongType
                  }
     deriving (Show)

data LitIntLongType = LitIntNotLong
                    | LitIntLong
                    | LitIntLongLong
     deriving (Show)

readLitInteger
   :: Bool
   -> Int
   -> Position
   -> String
   -> Either String (Either LitInteger LitUserDef)
readLitInteger isUD r@2  pos = rli isUD pos r . drop 2
readLitInteger isUD r@8  pos = rli isUD pos r
readLitInteger isUD r@10 pos = rli isUD pos r
readLitInteger isUD r@16 pos = rli isUD pos r . drop 2
readLitInteger _    r    _   = error $ "Constants.readLitInteger: unhandled radix: " ++ show r

rli
   :: Bool
   -> Position
   -> Int
   -> String
   -> Either String (Either LitInteger LitUserDef)
rli isUD pos radix' str =
   case readInt radix' i j str' of
      [(n', suffix)] -> 
         case isUD of
            False -> Right $ Left g
            True  -> Right $ Right $ LitUserInteger g $ mkIdent pos str0 (Name 0)  -- MCNFIXME: (Name 0) bogus
         where
            (_,isU,long',_,cnt)
               = foldl f (0,0,LitIntNotLong,0,0) suffix
            f :: (Int,Int,LitIntLongType,Int,Int) -> Char -> (Int,Int,LitIntLongType,Int,Int)
            f (0,0,lt           ,p,n) 'u' = (0,1,lt            ,p,n+1)
            f (0,0,lt           ,p,n) 'U' = (0,1,lt            ,p,n+1)
            f (0,u,LitIntNotLong,_,n) 'l' = (0,u,LitIntLong    ,0,n+1)
            f (0,u,LitIntNotLong,_,n) 'L' = (0,u,LitIntLong    ,1,n+1)
            f (0,u,LitIntLong   ,0,n) 'l' = (0,u,LitIntLongLong,0,n+1)
            f (0,u,LitIntLong   ,1,n) 'L' = (0,u,LitIntLongLong,0,n+1)
            f (_,u,l            ,p,n) _   = (1,u,l             ,p,n  )
            str0 = reverse $ drop cnt $ reverse suffix
            g = mkLitInteger (fromIntegral n') radix' (toEnum isU) long'
      parseFailed    -> error $ "Bad base-" ++ show radix' ++ " integer literal: " ++ show parseFailed
   where
      str' = filter (\c -> c /= '\'') str
      i x = j x < radix'
      j x = fromMaybe mAX_RADIX $ elemIndex x "0123456789abcdef"
      mAX_RADIX = 16

mkLitInteger
   :: Integer
   -> Int
   -> Bool
   -> LitIntLongType
   -> LitInteger
mkLitInteger i radix' isU long' = LitInteger i radix' $ mkIntType isU long'

mkIntType
   :: Bool
   -> LitIntLongType
   -> LitIntType
mkIntType = LitIntType

-- ----------------------------------------------------------------------------

data LitChar = LitChar {
                    v            :: !Char
                  , litCharType  :: !LitCharType
               }
             | LitChars {
                    vs           :: ![Char]
                  , litCharType  :: !LitCharType
               }
     deriving (Show)

data LitCharType = LitCharOrdinary
                 | LitCharWide
                 | LitChar16T
                 | LitChar32T
     deriving (Show)

readLitChar
   :: Bool
   -> Bool
   -> LitCharType
   -> Position
   -> String
   -> Either LitChar LitUserDef
readLitChar False False t@LitCharOrdinary _   str = (Left  . mkLitChar  t . fst . unescapeChar         . init . tail) str
readLitChar False True  t@LitCharOrdinary _   str = (Left  . mkLitChars t       . unescapeChars        . init . tail) str
readLitChar False False t@LitChar16T      _   str = (Left  . mkLitChar  t . fst . unescapeChar  . tail . init . tail) str
readLitChar False False t@LitChar32T      _   str = (Left  . mkLitChar  t . fst . unescapeChar  . tail . init . tail) str
readLitChar False False t@LitCharWide     _   str = (Left  . mkLitChar  t . fst . unescapeChar  . tail . init . tail) str
readLitChar False True  t@LitCharWide     _   str = (Left  . mkLitChars t       . unescapeChars . tail . init . tail) str
readLitChar True  False t@LitCharOrdinary pos str =  Right $ mkLitUserChar (mkLitChar  t $ fst $ unescapeChar         $ init $ tail $ stripId str) (mkIdentCharLit pos str)
readLitChar True  True  t@LitCharOrdinary pos str =  Right $ mkLitUserChar (mkLitChars t       $ unescapeChars        $ init $ tail $ stripId str) (mkIdentCharLit pos str)
readLitChar True  False t@LitChar16T      pos str =  Right $ mkLitUserChar (mkLitChar  t $ fst $ unescapeChar  $ tail $ init $ tail $ stripId str) (mkIdentCharLit pos str)
readLitChar True  False t@LitChar32T      pos str =  Right $ mkLitUserChar (mkLitChar  t $ fst $ unescapeChar  $ tail $ init $ tail $ stripId str) (mkIdentCharLit pos str)
readLitChar True  False t@LitCharWide     pos str =  Right $ mkLitUserChar (mkLitChar  t $ fst $ unescapeChar  $ tail $ init $ tail $ stripId str) (mkIdentCharLit pos str)
readLitChar True  True  t@LitCharWide     pos str =  Right $ mkLitUserChar (mkLitChars t       $ unescapeChars $ tail $ init $ tail $ stripId str) (mkIdentCharLit pos str)
readLitChar _     _     _                 _   _   = error "Constants.readLitChar: case failed"

mkIdentCharLit
   :: Position
   -> String
   -> Ident
mkIdentCharLit pos str = mkIdent pos (reverse $ take (f str) $ reverse str) (Name 0)
   where
      f = fromJust . elemIndex '\'' . reverse

stripId
   :: String
   -> String
stripId str = reverse $ drop (posOfQuote str) $ reverse str

posOfQuote
   :: String
   -> Int
posOfQuote = fromJust . elemIndex '\'' . reverse

unescapeChar
   :: String
   -> (Char, String)
unescapeChar ('\\':c':cs') = 
   case c' of
      'n'  -> ('\n', cs')
      't'  -> ('\t', cs')
      'v'  -> ('\v', cs')
      'b'  -> ('\b', cs')
      'r'  -> ('\r', cs')
      'f'  -> ('\f', cs')
      'a'  -> ('\a', cs')
      '\\' -> ('\\', cs')
      '?'  -> ('?' , cs')
      '\'' -> ('\'', cs')
      '"'  -> ('"' , cs')
      'x'  -> 
         case head' "bad escape sequence reading hex" (readHex cs') of
            (i, cs'') -> (toEnum i, cs'')
      _    -> 
         case head' "bad escape sequence reading octal" (readOct' (c':cs')) of
            (i, cs'') -> (toEnum i, cs'')
unescapeChar (   c':cs') = (c', cs')
unescapeChar []          = error $ "unescape char: empty string"

readOct'
   :: ReadS Int
readOct' s = map (\(i, cs) -> (i, cs ++ rest)) (readOct octStr)
   where
      octStr = takeWhile isOctDigit $ take 3 s
      rest   = drop (length octStr) s

head'
   :: String
   -> [a]
   -> a
head' err []  = error err
head' _ (x:_) = x

unescapeChars
   :: String
   -> String
unescapeChars [] = []
unescapeChars cs = 
   case unescapeChar cs of
      (c, cs') -> c : unescapeChars cs'

mkLitChar
   :: LitCharType
   -> Char
   -> LitChar
mkLitChar typ c = LitChar c typ

mkLitChars
   :: LitCharType
   -> [Char]
   -> LitChar
mkLitChars typ s = LitChars s typ

-- ----------------------------------------------------------------------------

data LitFloat = LitFloat {
                       litFloatValue   :: Double
                     , litFloatType    :: LitFloatType
               }
     deriving (Show)

data LitFloatType = LitFloatFloat
                  | LitFloatDouble
                  | LitFloatLongDouble
     deriving (Show)


readLitFloat
   :: Bool
   -> Position
   -> String
   -> Either LitFloat LitUserDef 
readLitFloat ud pos str = 
   case readFloat str' of
      [(n', suffix)] -> 
         case ud of
            True  -> Right $ mkLitUserFloat i j
            False -> Left i
         where
            i = mkLitFloat n' $ h suffix
            j = mkIdent pos m (Name 0)
            m =
               case h suffix of
                  LitFloatDouble -> suffix
                  _              -> tail suffix
            h ('f':_) = LitFloatFloat
            h ('F':_) = LitFloatFloat
            h ('l':_) = LitFloatLongDouble
            h ('L':_) = LitFloatLongDouble
            h _       = LitFloatDouble
      parseFailed    -> error $ "Bad base++  integer literal: " ++ show parseFailed
   where
      str' =
         g $ case isPrefixOf "." str of
            True  -> f ('0':str)
            False -> f str
      f x = filter (\c -> c /= '\'') x
      g x =
         case (last $ fst h) == '.' of          -- readFloat won't parse 1.e20
            True  -> fst h ++ "0" ++ snd h      -- without the 0 after the .
            False -> x                          -- so we add it if its not there.
         where
            h = span (\c -> c /= 'e' && c /= 'E') x

mkLitFloat
   :: Double
   -> LitFloatType
   -> LitFloat
mkLitFloat = LitFloat

-- ----------------------------------------------------------------------------

data LitString = LitString {
                       litStringValue     :: [Char]
                     , litStringType      :: LitStringType
                     , litStringRaw       :: Bool
                 }
     deriving (Show)

data LitStringType = LitStringOrdinary
                   | LitStringUtf8
                   | LitStringChar16T
                   | LitStringChar32T
                   | LitStringWide
     deriving (Show)

readLitString
   :: Bool
   -> Bool
   -> Position
   -> String
   -> Either LitString LitUserDef
readLitString u r@False pos str = 
   case u of
      True  -> Right $ mkLitUserString lus $ mkIdent pos (id' str) (Name 0) 
      False -> Left  lus
   where
      lus = mkLitString (f str) (litStringType' str) r
      id' = reverse . takeWhile (/= '\"') . reverse
      f = reverse . tail . dropWhile (/= '\"') . reverse . unescapeChars . tail . dropWhile (/= '\"')
readLitString u r@True  pos str = 
   case u of
      True  -> Right $ mkLitUserString lus $ mkIdent pos (id' str) (Name 0)
      False -> Left  lus
   where
      lus = mkLitString str' (litStringType' str) r 
      id' = reverse . takeWhile (/= '\"') . reverse
      f = span (/= '(') . tail . dropWhile (/= '\"')
      delimL = fst $ f str
      delimR = reverse $ fst $ span (/= ')') $ tail $ dropWhile (/= '\"') $ reverse $ snd $ f str
      str' =
         case delimL /= delimR of
            True  -> error "delimiters on raw string don't match"
            False -> reverse $ drop ((length delimL) + 1) $ reverse $ tail $ snd $ f str

litStringType'
   :: String
   -> LitStringType
litStringType' ('u':'8':_) = LitStringUtf8
litStringType' (    'u':_) = LitStringChar16T
litStringType' (    'U':_) = LitStringChar32T
litStringType' (    'L':_) = LitStringWide
litStringType' (_        ) = LitStringOrdinary

mkLitString
   :: String
   -> LitStringType
   -> Bool
   -> LitString
mkLitString = LitString

-- ----------------------------------------------------------------------------

data LitUserDef = LitUserInteger {
                          litUserInt      :: LitInteger
                        , litUserIdent    :: !Ident
                  }
                | LitUserFloat {
                          litUserFloat    :: LitFloat
                        , litUserIdent    :: !Ident
                  }
                | LitUserString {
                          litUserString   :: LitString
                        , litUserIdent    :: !Ident
                  }
                | LitUserChar {
                          litUserChar     :: LitChar
                        , litUserIdent    :: !Ident
                  }
     deriving (Show)

mkLitUserFloat
   :: LitFloat
   -> Ident
   -> LitUserDef
mkLitUserFloat = LitUserFloat

mkLitUserString
   :: LitString
   -> Ident
   -> LitUserDef
mkLitUserString = LitUserString

mkLitUserChar
   :: LitChar
   -> Ident
   -> LitUserDef
mkLitUserChar = LitUserChar

{-
instance Show LitString where
    showsPrec _ (LitString str wideflag) = _showWideFlag wideflag . showStringLit str

-- | @showCharConst c@ prepends _a_ String representing the C char constant corresponding to @c@.
-- If necessary uses octal or hexadecimal escape sequences.
showCharConst :: Char -> ShowS
showCharConst c = sQuote $ escapeLitChar c

_showWideFlag :: Bool -> ShowS
_showWideFlag flag = if flag then showString "L" else id

-- | get the haskell representation of a char constant
getLitChar :: LitChar -> [Char]
getLitChar (LitChar  c _)   = [c]
getLitChar (LitChars  cs _) = cs

-- | get integer value of a C char constant
-- undefined result for multi-char char constants
getLitCharAsInt :: LitChar -> Integer
getLitCharAsInt (LitChar c _) = fromIntegral (fromEnum c)
getLitCharAsInt (LitChars _cs _) = error "integer value of multi-character character constants is implementation defined"

-- | return @true@ if the character constant is /wide/.
isWideChar :: LitChar -> Bool
isWideChar (LitChar _ wideFlag) = wideFlag
isWideChar (LitChars _ wideFlag) = wideFlag

{-# SPECIALIZE setFlag :: LitIntFlag -> Flags LitIntFlag -> Flags LitIntFlag #-}
{-# SPECIALIZE clearFlag :: LitIntFlag -> Flags LitIntFlag -> Flags LitIntFlag #-}
{-# SPECIALIZE testFlag :: LitIntFlag -> Flags LitIntFlag -> Bool #-}


getLitInteger :: LitInteger -> Integer
getLitInteger (LitInteger i _ _) = i

cFloat :: Float -> LitFloat
cFloat = LitFloat . show

-- dummy implementation
readLitFloat :: String -> LitFloat
readLitFloat = LitFloat

-- construction
cString :: String -> LitString
cString str = LitString str False
cString_w :: String -> LitString
cString_w str = LitString str True

-- selectors
getLitString :: LitString -> String
getLitString (LitString str _) = str
isWideString :: LitString -> Bool
isWideString (LitString _ wideflag) = wideflag

-- | concatenate a list of C string literals
concatLitStrings :: [LitString] -> LitString
concatLitStrings cs = LitString (concatMap getLitString cs) (any isWideString cs)

-- | @showStringLiteral s@ prepends a String representing the C string literal corresponding to @s@.
-- If necessary it uses octal or hexadecimal escape sequences.
showStringLit :: String -> ShowS
showStringLit = dQuote . concatMap showStringChar
  where
  showStringChar c | isSChar c = return c
                     | c == '"'  = "\\\""
                     | otherwise = escapeChar c



-- | @isAsciiSourceChar b@ returns @True@ if the given character is a character which
--   may appear in a ASCII C source file and is printable.
isAsciiSourceChar :: Char -> Bool
isAsciiSourceChar c = isAscii c && isPrint c

-- | @isLitChar c@ returns true, if c is a source character which does not have to be escaped in
--   C char constants (C99: 6.4.4.4)
isLitChar :: Char -> Bool
isLitChar '\\' = False
isLitChar '\'' = False
isLitChar '\n' = False
isLitChar c = isAsciiSourceChar c

-- | @escapeLitChar c@ escapes c for use in a char constant
escapeLitChar :: Char -> String
escapeLitChar '\'' = "\\'"
escapeLitChar c | isLitChar c = [c]
              | otherwise = escapeChar c

-- | @isSChar c@ returns true if c is a source character which does not have to be escaped in C string
--  literals (C99: 6.4.5)
isSChar :: Char -> Bool
isSChar '\\' = False
isSChar '\"' = False
isSChar '\n' = False
isSChar c = isAsciiSourceChar c

showOct' :: Int -> String
showOct' i = replicate (3 - length s) '0' ++ s
  where s = showOct i ""

escapeChar :: Char -> String
escapeChar '\\' = "\\\\"
escapeChar '\a' = "\\a"
escapeChar '\b' = "\\b"
escapeChar '\ESC' = "\\e";
escapeChar '\f' = "\\f"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar '\v' = "\\v"
escapeChar c  | (ord c) < 512   = '\\' : showOct' (ord c)
              | otherwise       = '\\' : 'x'  : showHex (ord c) ""
-}
unescapeString :: String -> String
unescapeString [] = []
unescapeString cs = case unescapeChar cs of
                        (c, cs') -> c : unescapeString cs'

-- helpers
sQuote :: String -> ShowS
sQuote s t = "'" ++ s ++ "'" ++ t
dQuote :: String -> ShowS
dQuote s t = ('"' : s) ++ "\"" ++ t

{-
-- TODO: Move to separate file ?
newtype Flags f = Flags Integer deriving (Eq,Ord,Data,Typeable)
noFlags :: Flags f
noFlags = Flags 0
setFlag :: (Enum f) => f -> Flags f -> Flags f
setFlag flag (Flags k)   = Flags$ k  `setBit` fromEnum flag
clearFlag :: (Enum f) => f -> Flags f -> Flags f
clearFlag flag (Flags k) = Flags$ k `clearBit` fromEnum flag
testFlag :: (Enum f) => f -> Flags f -> Bool
testFlag flag (Flags k)  = k `testBit` fromEnum flag
-}
