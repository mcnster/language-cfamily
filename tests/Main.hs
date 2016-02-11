module Main where

import Language.CFamily.C.Builtin
import Language.CFamily.CXX.Lexer
import Language.CFamily.Token
import Language.CFamily.Data.Error
import Language.CFamily.Data.Ident    (Ident)
import Language.CFamily.Data.InputStream
import Language.CFamily.Data.Name
import Language.CFamily.Data.ParserMonad
import Language.CFamily.Data.Position

import Control.Monad
import qualified Data.Set as Set (fromList, insert, member, delete)
import Debug.Trace
import System.Environment
import System.IO

{-
newtype ParseError = ParseError ([String],Position)

instance Show ParseError where
    show (ParseError (msgs,pos)) = showErrorInfo "Syntax Error !" (ErrorInfo LevelError pos msgs)

data ParseResult a = POk     !PState a
                   | PFailed [String] Position   -- The error message and position

data PState = PState {
        curPos     :: !Position,       -- position at current input location
        curInput   :: !String,         -- the current input
        prevToken  ::  Token,          -- the previous token
        savedToken ::  Token           -- and the token before that
     }

newtype P a = P {
                     unP :: PState -> ParseResult a
              }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = return
  (<*>) = ap

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail m = getPos >>= \pos -> failP pos [m]

returnP
   :: a 
   -> P a
returnP a = P $ \s -> POk s a

thenP
   :: P a 
   -> (a -> P b) 
   -> P b
(P m) `thenP` k = P $ \s ->
   case m s of
      POk s' a        -> (unP (k a)) s'
      PFailed err pos -> PFailed err pos

failP
   :: Position 
   -> [String] 
   -> P a
failP pos msg = P $ \_ -> PFailed msg pos

getPos
   :: P Position
getPos = P $ \s@PState{curPos=pos} -> POk s pos
-}
-- ----------------------------------------------------------------------------

main = do
   args <- getArgs
   let
      fn = last args
   s <- readInputStream fn
   let
      lexed = lexCXX s $ initPos fn
   putStrLn $ show lexed


lexCXX
   :: InputStream
   -> Position
   -> [Token]
lexCXX str pos = exParser lexToken str pos builtinTypeNames (namesStartingFrom 0)
{-
exParser
   :: P a 
   -> InputStream 
   -> Position 
   -> [Ident] 
   -> [Name]
   -> Either ParseError (a,PState)
exParser (P parser) input pos builtins names =
   case parser initialState of
      PFailed message errpos -> Left (ParseError (message,errpos))
      POk st result -> Right (result, st)
   where 
      initialState = PState {
          curPos = pos,
          curInput = input,
          prevToken = internalErr "CLexer.execParser: Touched undefined token!",
          savedToken = internalErr "CLexer.execParser: Touched undefined token (safed token)!",
          namesupply = names,
          tyidents = Set.fromList builtins,
          scopes   = []
        }
-}
exParser
   :: P Token
   -> InputStream
   -> Position
   -> [Ident]
   -> [Name]
   -> [Token]
exParser p input pos builtins names = bar p initialState []
   where
      initialState = PState {
              curPos     = pos
            , curInput   = input
            , prevToken  = internalErr "touched undefined token!"
            , savedToken = internalErr "touched undefined token!"
            , namesupply = names
            , tyidents   = Set.fromList builtins
            , scopes     = []
      }

bar
   :: P Token
   -> PState
   -> [Token]
   -> [Token]
bar p@(P parser) state rs =
   case parser state of
      PFailed m e -> error $ "parse error: " ++ head m ++ show e
      POk st r ->
         case r of
            TokEof   -> rs
            t        -> bar p st (rs ++ [t])

