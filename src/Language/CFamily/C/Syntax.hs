-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CFamily.C.Syntax
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Syntax of C files: The abstract syntax tree and constants.
-----------------------------------------------------------------------------
module Language.CFamily.C.Syntax (
     -- * Constants
     module Language.CFamily.C.Syntax.Constants,
     -- * Syntax tree
     module Language.CFamily.C.Syntax.AST,
)
where
import Language.CFamily.C.Syntax.AST
import Language.CFamily.C.Syntax.Constants
