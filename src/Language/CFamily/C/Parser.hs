{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CFamily.C.Parser
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Language.C parser
-----------------------------------------------------------------------------
module Language.CFamily.C.Parser (
    -- * Simple API
    parseC,
    -- * Parser Monad
    P,execParser,execParser_,builtinTypeNames,
    -- * Exposed Parsers
    translUnitP, extDeclP, statementP, expressionP,
    -- * Parser Monad
    ParseError(..)
)
where
import Language.CFamily.C.Parser.Parser (parseC,translUnitP, extDeclP, statementP, expressionP)
import Language.CFamily.C.Parser.Builtin (builtinTypeNames)
import Language.CFamily.Data

-- | run the given parser using a new name supply and builtin typedefs
--   see 'execParser'
--
-- Synopsis: @runParser parser inputStream initialPos@
execParser_ :: P a -> InputStream -> Position -> Either ParseError a
execParser_ parser input pos =
  fmap fst $ execParser parser input pos builtinTypeNames newNameSupply
