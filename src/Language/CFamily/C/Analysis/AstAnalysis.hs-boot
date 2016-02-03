module Language.CFamily.C.Analysis.AstAnalysis where

import Language.CFamily.C.Analysis.SemRep
import Language.CFamily.Data

import Language.CFamily.C.Syntax.AST

data StmtCtx = FunCtx VarDecl
             | LoopCtx
             | SwitchCtx

data ExprSide = LValue | RValue

tExpr :: MonadTrav m => [StmtCtx] -> ExprSide -> CExpr -> m Type
