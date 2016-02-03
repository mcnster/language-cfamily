-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CFamily.C.Analysis
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  ghc
--
-- Analysis of the AST.
--
-- Currently, we provide a monad for analysis and analyze declarations and types.
-- Especially note that there is no direct support for analyzing function bodies and
-- constant expressions.
--
-- /NOTE/ This is an experimental interface, and therefore the API will change in the
-- future.
--
-- DONE:
--
--  * Name analysis framework
--
--  * File-scope analysis
--
--  * Declaration analysis
--
-- TODO:
--
--  * Type checking expressions
--
--  * Constant expression evaluation (CEE)
--
--  * Typed representation of attributes (depends on CEE)
--
--  * Normalized representation of initializers
--
--  * Support for analyzing function bodies (depends on CEE)
--
--  * Normalizing expressions and statements
--
--  * Formal rules how to link back to the AST using NodeInfo fields
--
--  * Typed assembler representation

-----------------------------------------------------------------------------
module Language.CFamily.C.Analysis (
    -- * Semantic representation
    module Language.CFamily.C.Analysis.SemRep,
    -- * Error datatypes for the analysis
    module Language.CFamily.C.Analysis.SemError,
    -- * Top level analysis
    module Language.CFamily.C.Analysis.AstAnalysis,
    -- * Analyzing declarations
    module Language.CFamily.C.Analysis.DeclAnalysis,
    -- * Debug print
    module Language.CFamily.C.Analysis.Debug,
    -- * Traversal monad
    module Language.CFamily.Data.TravMonad
)
where
import Language.CFamily.C.Analysis.SemError
import Language.CFamily.C.Analysis.SemRep
import Language.CFamily.C.Analysis.AstAnalysis
import Language.CFamily.C.Analysis.DeclAnalysis

import Language.CFamily.C.Analysis.Debug

import Language.CFamily.Data.TravMonad
