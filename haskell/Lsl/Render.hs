module Lsl.Render(renderLSLScript,renderCompiledScript) where

import Lsl.Structure
import Lsl.Util

renderLSLScript library lslScript = 
    case validLSLScript library lslScript of
        Invalid s -> Invalid s
        Valid x -> Valid $ renderCompiledScript "" x
        
renderCompiledScript stamp (globals,funcs,states) =
   renderString "// LSL script generated: " . renderString stamp . renderString "\n" .
   renderGlobals globals . renderFuncs funcs . renderStates states

renderSequence r = (foldl (.) blank) . (map r)

renderGlobals = renderSequence renderGlobal

renderGlobal (GDecl var val) = renderVar var . 
    case val of 
        Nothing -> renderString ";\n"
        Just expr -> renderString " = " . renderSimple expr . renderString ";\n"

renderCtxSimple (Ctx _ expr) = renderSimple expr
renderSimple (Neg expr) = renderChar '-' . renderCtxExpr expr
renderSimple (ListExpr l) =
    renderChar '[' .
        (foldl (.) id $ separateWith (renderChar ',') $ map renderCtxSimple l) .
        renderChar ']'
renderSimple (VecExpr x y z) = renderChar '<' . renderCtxSimple x .
                               renderChar ',' . renderCtxSimple y .
                               renderChar ',' . renderCtxSimple z .
                               renderChar '>'
renderSimple (RotExpr x y z s) = renderChar '<' . renderCtxSimple x .
                                 renderChar ',' . renderCtxSimple y .
                                 renderChar ',' . renderCtxSimple z .
                                 renderChar ',' . renderCtxSimple s .
                                 renderChar '>'
renderSimple e = renderExpression e

renderStates = renderSequence renderState

renderState (State (Ctx _ "default") handlers) =
    renderString "default {\n" . renderHandlers handlers . renderString "}\n"
renderState (State (Ctx _ name) handlers) =
    renderString "state " . renderString name . renderString " {\n" . renderHandlers handlers . renderString "}\n"
 
renderHandlers = renderSequence renderHandler

renderHandler (Handler (Ctx _ name) vars stmts) = renderHandler' name vars stmts

renderHandler' name vars stmts =
    renderIndent 0 . renderString name . renderChar '(' . renderVarList (ctxItems vars) . renderString ") {\n" . 
        renderStatements 1 stmts . renderIndent 0 . renderString "}\n"
        
renderChar = showChar
renderVar (Var nm t) = (renderType t) . renderChar ' ' . (renderString nm)
renderFuncDec (FuncDec name t vars) = 
    let sp = if t == LLVoid then "" else " " in
        renderType t . renderString sp . renderString (ctxItem name) . renderChar '(' . 
        renderVarList (ctxItems vars) . renderChar ')'

renderVarList [] = blank
renderVarList (v:vars) = 
    (renderVar v) .
        let render' [] = blank
            render' (v:vars) = renderChar ',' . renderVar v . render' vars in render' vars

renderFuncs = renderSequence renderFunc

renderFunc (Func dec stmts) = 
    renderFuncDec dec . renderString "{\n" . renderStatements 0 stmts . renderString "}\n"

renderIndent 0 = renderString "    "
renderIndent n = renderString "    " . renderIndent (n - 1)

renderCtxStatement hang n (Ctx _ s) = renderStatement hang n s

renderStatements n = renderSequence (renderCtxStatement False n)
    
doHang True n = renderString " "
doHang False n = renderIndent n

renderOptionalExpression Nothing = blank
renderOptionalExpression (Just expr) = renderCtxExpr expr

renderStatement hang n stmt = doHang hang n . renderStatement' n stmt
renderStatement' n (Compound stmts) = 
        renderString "{\n" . renderStatements (n+1) stmts . renderIndent n . renderString "}\n"
renderStatement' n (While expr stmt) = 
    renderString "while (" . renderCtxExpr expr . renderChar ')' . renderStatement True n stmt
renderStatement' n (DoWhile stmt expr) = 
    renderString "do " . renderStatement True n stmt . renderString " while (" . renderCtxExpr expr . renderChar ')'
renderStatement' n (For mexpr1 mexpr2 mexpr3 stmt) =
    renderString "for (" . renderCtxExprs "" mexpr1 . renderString "; " . renderOptionalExpression mexpr2 .
    renderString "; " . renderCtxExprs "" mexpr3 . renderString ")" . renderStatement True n stmt
renderStatement' n (If expr stmt1 stmt2) =
    renderString "if (" . renderCtxExpr expr . renderChar ')' . renderStatement True n stmt1 . 
        case stmt2 of 
            NullStmt -> blank
            _ -> renderIndent n . renderString "else " . (renderStatement True n stmt2)
renderStatement' n (Decl var val) = 
    renderVar var . 
        case val of 
            Nothing -> renderString ";\n"
            Just expr -> renderString " = " . renderCtxExpr expr . renderString ";\n"
renderStatement' n (NullStmt) = blank
renderStatement' n (Return Nothing) = renderString "return;\n"
renderStatement' n (Return (Just expr)) = renderString "return " . renderCtxExpr expr . renderString ";\n";
renderStatement' n (StateChange name) = renderString "state " . renderString name . renderString ";\n";
renderStatement' n (Do expr) = renderCtxExpr expr . renderString ";\n";
renderStatement' n (Label s) = renderChar '@' . renderString s . renderString ";\n";
renderStatement' n (Jump s) = renderString "jump " . renderString s . renderString ";\n";

renderExpressions prefix [] = blank
renderExpressions prefix (e:es) = renderString prefix . renderExpression e . renderExpressions "," es

renderCtxName (Ctx _ n) = renderString n
renderCtxExpr (Ctx _ e) = renderExpression e

renderCtxExprs prefix [] = blank
renderCtxExprs prefix (e:es) = renderString prefix . renderCtxExpr e . renderCtxExprs "," es

renderExpression (IntLit i) = shows i
renderExpression (FloatLit f) = shows f
renderExpression (StringLit s) = shows s
renderExpression (KeyLit k) = shows k
renderExpression (VecExpr x y z) = 
    renderChar '<' . renderCtxExpr x . renderChar ',' .
                     renderCtxExpr y . renderChar ',' .
                     renderCtxExpr z . renderChar '>'
renderExpression (RotExpr x y z s) = 
    renderChar '<' . renderCtxExpr x . renderChar ',' .
                     renderCtxExpr y . renderChar ',' .
                     renderCtxExpr z . renderChar ',' .
                     renderCtxExpr s . renderChar '>'
renderExpression (ListExpr l) = 
    let r prefix [] = blank
        r prefix (i:is) = renderString prefix . renderCtxExpr i . r "," is
    in renderChar '[' . r "" l . renderChar ']'
renderExpression (Add expr1 expr2) = renderBinExpr "+" expr1 expr2
renderExpression (Sub expr1 expr2) = renderBinExpr "-" expr1 expr2
renderExpression (Mul expr1 expr2) = renderBinExpr "*" expr1 expr2
renderExpression (Div expr1 expr2) = renderBinExpr "/" expr1 expr2
renderExpression (Mod expr1 expr2) = renderBinExpr "%" expr1 expr2
renderExpression (BAnd expr1 expr2) = renderBinExpr "&" expr1 expr2
renderExpression (Xor expr1 expr2) = renderBinExpr "^" expr1 expr2
renderExpression (BOr expr1 expr2) = renderBinExpr "|" expr1 expr2
renderExpression (Lt expr1 expr2) = renderBinExpr "<" expr1 expr2
renderExpression (Gt expr1 expr2) = renderBinExpr ">" expr1 expr2
renderExpression (Le expr1 expr2) = renderBinExpr "<=" expr1 expr2
renderExpression (Ge expr1 expr2) = renderBinExpr ">=" expr1 expr2
renderExpression (And expr1 expr2) = renderBinExpr "&&" expr1 expr2
renderExpression (Or expr1 expr2) = renderBinExpr "||" expr1 expr2
renderExpression (ShiftL expr1 expr2) = renderBinExpr "<<" expr1 expr2
renderExpression (ShiftR expr1 expr2) = renderBinExpr ">>" expr1 expr2
renderExpression (Inv expr) = renderChar '(' . renderChar '~' . renderCtxExpr expr . renderChar ')'
renderExpression (Not expr) = renderChar '(' . renderChar '!' . renderCtxExpr expr . renderChar ')'
renderExpression (Neg expr) = renderChar '(' . renderChar '-' . renderCtxExpr expr . renderChar ')'
renderExpression (Call name exprs) = renderCtxName name . renderChar '(' . renderCtxExprs "" exprs . renderChar ')'
renderExpression (Cast t expr) = renderString "((" . renderType t . renderChar ')' . renderCtxExpr expr . renderChar ')'
renderExpression (Get var) = renderVarAccess var
--renderExpression (Const var) = renderVarAccess var
renderExpression (Set var expr) = renderVarAccess var . renderString " = " . renderCtxExpr expr
renderExpression (IncBy va expr) = renderAssignment va "+=" expr
renderExpression (DecBy va expr) = renderAssignment va "-=" expr
renderExpression (MulBy va expr) = renderAssignment va "*=" expr
renderExpression (DivBy va expr) = renderAssignment va "/=" expr
renderExpression (ModBy va expr) = renderAssignment va "%=" expr
renderExpression (Equal expr1 expr2) = renderBinExpr "==" expr1 expr2
renderExpression (NotEqual expr1 expr2) = renderBinExpr "!=" expr1 expr2
renderExpression (PostInc va) = renderVarAccess va . renderString "++"
renderExpression (PostDec va) = renderVarAccess va . renderString "--"
renderExpression (PreInc va) = renderString "++" . renderVarAccess va
renderExpression (PreDec va) = renderString "--" . renderVarAccess va 

renderBinExpr op expr1 expr2 = renderChar '(' . renderCtxExpr expr1 . renderChar ' ' .
                               renderString op . renderChar ' ' . renderCtxExpr expr2 . renderChar ')'
renderAssignment va op expr = renderVarAccess va . renderChar ' ' . renderString op . renderChar ' ' . renderCtxExpr expr
renderComponent All = blank
renderComponent X = renderString ".x"
renderComponent Y = renderString ".y"
renderComponent Z = renderString ".z"
renderComponent S = renderString ".s"

renderVarAccess (v,c) = renderCtxName v . renderComponent c

renderString s s' = s ++ s'
renderType LLList = renderString "list"
renderType LLInteger = renderString "integer"
renderType LLVector = renderString "vector"
renderType LLFloat = renderString "float"
renderType LLString = renderString "string"
renderType LLRot = renderString "rotation"
renderType LLKey = renderString "key"
renderType LLVoid = blank

blank :: String -> String
blank = id