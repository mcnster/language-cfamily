module Language.CFamily.Lexer where

import Language.CFamily.Constants
import Language.CFamily.Data
import Language.CFamily.Token

import Control.Monad (liftM)

import Data.Char (chr, isDigit)
import Data.Word (Word8)

{-
-- Fix the 'octal' lexing of '0'
readCOctal
   :: String
   -> Either String CInteger
readCOctal s@('0':r) =
   case r of
      (c:_) | isDigit c -> readCInteger OctalRepr r
      _                 -> readCInteger DecRepr s
readCOctal _ = error "Lexer.x:readCOctal unhandled pattern"
-}

tok
   :: Int
   -> (PosLength -> Token)
   -> Position
   -> P Token
tok len tc pos = return (tc (pos,len))

adjustLineDirective
   :: Int
   -> String
   -> Position
   -> Position
adjustLineDirective pragmaLen str pos =
   offs' `seq` fname' `seq` row' `seq` (position offs' fname' row' 1)
   where
      offs'           = (posOffset pos) + pragmaLen
      str'            = dropWhite . drop 1 $ str
      (rowStr, str'') = span isDigit str'
      row'            = read rowStr
      str'''          = dropWhite str''
      fnameStr        = takeWhile (/= '"') . drop 1 $ str'''
      fname           = posFile pos
      dropWhite       = dropWhile (\c -> c == ' ' || c == '\t')
      fname'
         | null str''' || head str''' /= '"' = fname
         -- try and get more sharing of file name strings
         | fnameStr == fname                 = fname
         | otherwise                         = fnameStr

-- special utility for the lexer
unescapeMultiChars
   :: String
   -> [Char]
unescapeMultiChars cs@(_    : _ : _ ) = case unescapeChar cs of (c,cs') -> c : unescapeMultiChars cs'
unescapeMultiChars    ('\''     : []) = []
unescapeMultiChars _ = error "Unexpected end of multi-char constant"

{-# INLINE token_ #-}
-- token that ignores the string
token_
   :: Int
   -> (PosLength -> Token)
   -> Position
   -> Int
   -> InputStream
   -> P Token
token_ len tok' pos _ _ = return (tok' (pos,len))

{-# INLINE token_fail #-}
-- error token
token_fail
   :: String
   -> Position
   -> Int
   -> InputStream
   -> P Token
token_fail errmsg pos _ _ = failP pos [ "Lexical Error !", errmsg ]


{-# INLINE token #-}
-- token that uses the string
token
   :: (PosLength -> a -> Token)
   -> (String -> a)
   -> Position
   -> Int
   -> InputStream
   -> P Token
token tok' read' pos len str = return (tok' (pos,len) (read' $ takeChars len str))

{-# INLINE token_plus #-}
-- token that may fail
token_plus
   :: (PosLength -> a -> Token)
   -> (String -> Either String a)
   -> Position
   -> Int
   -> InputStream
   -> P Token
token_plus tok' read' pos len str =
   case read' (takeChars len str) of
      Left err -> failP pos [ "Lexical error ! ", err ]
      Right ok -> return $! tok' (pos,len) ok

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = (Position,         -- current position,
                  InputStream)      -- current input string

alexInputPrevChar
   :: AlexInput
   -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

-- for alex-3.0
alexGetByte
   :: AlexInput
   -> Maybe (Word8, AlexInput)
alexGetByte (p,is)
   | inputStreamEmpty is = Nothing
   | otherwise           =
      let (b,s) = takeByte is in
         -- this is safe for latin-1, but ugly
         let p' = alexMove p (chr (fromIntegral b)) in
            p' `seq` Just (b, (p', s))

alexGetChar
   :: AlexInput
   -> Maybe (Char,AlexInput)
alexGetChar (p,is)
   | inputStreamEmpty is = Nothing
   | otherwise           =
      let (c,s) = takeChar is in
         let p' = alexMove p c in
            p' `seq` Just (c, (p', s))

alexMove
   :: Position
   -> Char
   -> Position
alexMove pos ' '  = incPos    pos 1
alexMove pos '\n' = retPos    pos
alexMove pos '\r' = incOffset pos 1
alexMove pos _    = incPos    pos 1

lexicalError
   :: P a
lexicalError = do
   pos <- getPos
   (c,_) <- liftM takeChar getInput
   failP pos
         ["Lexical error !"
            , "The character " ++ show c ++ " does not fit here."]

parseError
   :: P a
parseError = do
   tok' <- getLastToken
   failP (posOf tok')
         ["Syntax error !"
            , "The symbol `" ++ show tok' ++ "' does not fit here."]

doIntegerLiteral
   :: Bool
   -> Int
   -> Position
   -> Int
   -> InputStream
   -> P Token
doIntegerLiteral u@False r p = token_plus TokLitInt     (f . readLitInteger u r p) p
   where
      f (Right (Left li)) = Right li
      f (Left str)        = Left  str
      f _                 = error "Lexer.doIntegerLiteral False"
doIntegerLiteral u@True  r p = token_plus TokLitUserDef (f . readLitInteger u r p) p
   where
      f (Right (Right lud)) = Right lud
      f (Left str)          = Left  str
      f _                   = error "Lexer.doIntegerLiteral True"

doCharLiteral
   :: Bool
   -> Bool
   -> LitCharType
   -> Position
   -> Int
   -> InputStream
   -> P Token
doCharLiteral u@False m t p = token TokLitChar    (f . readLitChar u m t p) p
   where
      f (Left x) = x
      f _        = error "Lexer.doCharLiteral False"

doCharLiteral u@True  m t p = token TokLitUserDef (f . readLitChar u m t p) p
   where
      f (Right x) = x
      f _         = error "Lexer.doCharLiteral True"

doFloatLiteral
   :: Bool
   -> Position
   -> Int
   -> InputStream
   -> P Token
doFloatLiteral u@False p = token TokLitFloat   (f . readLitFloat u p) p
   where
      f (Left x) = x
      f _        = error "Lexer.doFloatLiteral False"
doFloatLiteral u@True  p = token TokLitUserDef (f . readLitFloat u p) p
   where
      f (Right x) = x
      f _         = error "Lexer.doFloatLiteral True"

doStringLiteral
   :: Bool
   -> Bool
   -> Position
   -> Int
   -> InputStream
   -> P Token
doStringLiteral u@False r pos = token TokLitString  (f . readLitString u r pos) pos
   where
      f (Left x) = x
      f _        = error "Lexer.doStringLiteral False"
doStringLiteral u@True  r pos = token TokLitUserDef (f . readLitString u r pos) pos
   where
      f (Right x) = x
      f _         = error "Lexer.doStringLiteral True"

