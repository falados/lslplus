{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Language.Lsl.Internal.Optimize(optimizeScript) where

import Language.Lsl.Syntax(Expr(..),Statement(..),Global(..),Func(..),FuncDec(..),State(..),Ctx(..),Handler(..))

optimizeScript :: ([Global],[Func],[State]) -> ([Global],[Func],[State])
optimizeScript (gs,fs,ss) = (gs, fs', ss)
    where usedFuncs = concatMap stateUsesFuncs ss ++ concatMap funcUsesFuncs fs
          fs' = [ f | f@(Func fd _) <- fs, (ctxItem . funcName) fd `elem` usedFuncs ]

exprUsesFuncs :: Expr -> [String]
exprUsesFuncs (Call (ctxName) es) = ctxItem ctxName : (exprsUsesFuncs es)
exprUsesFuncs (IntLit _) = []
exprUsesFuncs (FloatLit _) = []
exprUsesFuncs (StringLit _) = []
exprUsesFuncs (KeyLit _) = []
exprUsesFuncs (ListExpr es) = exprsUsesFuncs es
exprUsesFuncs (VecExpr x y z) = exprsUsesFuncs [x,y,z]
exprUsesFuncs (RotExpr x y z s) = exprsUsesFuncs [x,y,z,s]
exprUsesFuncs (Add e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Sub e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Mul e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Div e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Mod e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Set _ e) = exprUsesFuncs' e
exprUsesFuncs (BAnd e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (BOr e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Xor e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (ShiftL e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (ShiftR e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (And e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Or e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Equal e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (NotEqual e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Lt e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Gt e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Le e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (Ge e1 e2) = exprsUsesFuncs [e1,e2]
exprUsesFuncs (IncBy _ e) = exprUsesFuncs' e
exprUsesFuncs (DecBy _ e) = exprUsesFuncs' e
exprUsesFuncs (MulBy _ e) = exprUsesFuncs' e
exprUsesFuncs (DivBy _ e) = exprUsesFuncs' e
exprUsesFuncs (ModBy _ e) = exprUsesFuncs' e
exprUsesFuncs (Not e) = exprUsesFuncs' e
exprUsesFuncs (Neg e) = exprUsesFuncs' e
exprUsesFuncs (Inv e) = exprUsesFuncs' e
exprUsesFuncs (Cast _ e) = exprUsesFuncs' e
exprUsesFuncs _ = []

exprUsesFuncs' = exprUsesFuncs . ctxItem
exprsUsesFuncs = concatMap exprUsesFuncs'

statementUsesFuncs :: Statement -> [String]
statementUsesFuncs (Compound ss) = concatMap (statementUsesFuncs . ctxItem) ss
statementUsesFuncs (While e s) = exprUsesFuncs' e ++ statementUsesFuncs s
statementUsesFuncs (DoWhile s e) = statementUsesFuncs s ++ exprUsesFuncs' e
statementUsesFuncs (For e1 me2 e3 s) = 
    exprsUsesFuncs e1 ++ maybe [] exprUsesFuncs' me2 ++ exprsUsesFuncs e3 ++  statementUsesFuncs s
statementUsesFuncs (If e s1 s2) = exprUsesFuncs' e ++ statementUsesFuncs s1 ++ statementUsesFuncs s2
statementUsesFuncs (Decl _ (Just e)) = exprUsesFuncs' e
statementUsesFuncs (Return (Just e)) = exprUsesFuncs' e
statementUsesFuncs (Do e) = exprUsesFuncs' e
statementUsesFuncs _ = []

statementUsesFuncs' = statementUsesFuncs . ctxItem    

funcUsesFuncs :: Func -> [String]
funcUsesFuncs (Func _ stmts) = concatMap statementUsesFuncs' stmts
handlerUsesFuncs :: Handler -> [String]
handlerUsesFuncs (Handler _ _ stmts) = concatMap statementUsesFuncs' stmts

stateUsesFuncs :: State -> [String]
stateUsesFuncs (State _ handlers) = concatMap handlerUsesFuncs handlers
