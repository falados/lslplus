module Language.Lsl.Parse(
        parseScript,
        parseModule,
        exprParser,
        parseType,
        parseScriptFromString,
        parseModuleFromString,
        parseScriptFromStringAQ,
        parseModuleFromStringAQ
    ) where

import Data.Char(digitToInt)
import Data.List(intersperse)
import Language.Lsl.Syntax(Expr(..),Statement(..),Func(..),FuncDec(..),Handler(..),State(..),Ctx(..),SourceContext(..),LSLType(..),
                  Component(..),Var(..),LModule(..),LSLScript(..),GlobDef(..),goodHandlers)
import Text.ParserCombinators.Parsec hiding (State)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Text.ParserCombinators.Parsec.Error
import Control.Monad.Error(liftIO)
import Control.Monad.Trans(MonadIO)

-- define basic rules for lexical analysis
lslStyle = javaStyle
             { P.reservedOpNames= ["*","/","+","++","-","--","^","&","&&",
                                   "|","||","==","=","!=","<","<=",">=",">",
                                   "<<",">>","!","%","~","@","+=","-=","*=","/=","%="],
               P.reservedNames = ["state","default","string","integer","list","vector","rotation","key","float","if","else",
                                "while","for","do","jump","return","default", "$import", "$module","quaternion"],
               P.caseSensitive = True,
               P.identStart = letter <|> char '_',
               P.opLetter = oneOf "*/+:!#$%&*+./=?@\\^|-~",
               P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~" }
lexer :: P.TokenParser Bool
lexer  = P.makeTokenParser lslStyle

identLetter = P.identLetter lslStyle
               
identifier       = P.identifier lexer
reserved         = P.reserved lexer
operator         = P.operator lexer
reservedOp       = P.reservedOp lexer
                        
charLiteral      = P.charLiteral lexer
-- stringLiteral    = P.stringLiteral lexer
natural          = P.natural lexer
integer          = P.integer lexer
float            = P.float lexer
--naturalOrFloat   = P.naturalOrFloat lexer
decimal          = P.decimal lexer
hexadecimal      = P.hexadecimal lexer
octal            = P.octal lexer

symbol           = P.symbol lexer
lexeme           = P.lexeme lexer
whiteSpace       = P.whiteSpace lexer
             
parens           = P.parens lexer 
braces           = P.braces lexer
angles           = P.angles lexer
brackets         = P.brackets lexer
semi             = P.semi lexer
comma            = P.comma lexer
colon            = P.colon lexer
dot              = P.dot lexer
semiSep          = P.semiSep lexer
semiSep1         = P.semiSep1 lexer
commaSep         = P.commaSep lexer
commaSep1        = P.commaSep1 lexer

decimalFraction w = do char '.'
                       fracPart True w
                
fracPart reqDigit w = do digits <- (if reqDigit then many1 else many) digit <?> "fractional part of decimal"
                         p <- option 1.0 expon
                         return $ p * (w + (foldr (\ b d -> (b + d) / 10.0) 0 $ map (fromIntegral.digitToInt) digits))

expon = do oneOf "eE"
           s <- option '+' (oneOf "+-")
           let k x = if s == '+' then x else 1/x
           digits <- many1 digit <?> "exponent"
           let p = foldl (\ b d -> b * 10 + d) 0 $ map digitToInt digits
           return ((k (10^p))::Float)

hex = do oneOf "xX"
         digits <- many1 hexDigit <?> "hex digit"
         return (foldl (\ b d -> b * 16 + d) 0 $ map digitToInt digits)
-- lsl doesn't support octal!
-- oct = do oneOf "oO"
--          digits <- many1 octDigit <?> "octal digit"
--          return (foldl (\ b d -> b * 8 + d) 0 $ map digitToInt digits)

-- Like Parsec 'naturalOrFloat', but accept <digits>. and .<digits><exp> as valid floating point numbers
naturalOrFloat = do v <- natOrFloat <?> "number"
                    whiteSpace
                    return v
natOrFloat =  (decimalFraction 0.0 >>= return . Right)
           <|> do char '0'
                  option  (Left 0) prefZeroNum
           <|> decimalOrFloat

prefZeroNum = (hex >>= return . Left)
          <|> (decimalFraction 0.0 >>= return . Right)
          <|> decimalOrFloat
decimalOrFloat =
    do wholeDigits <- many1 digit <?> "number"
       let w = foldl (\ b d -> b * 10 + d) 0 $ map digitToInt wholeDigits
       mf <- option Nothing (char '.' >> option (fromIntegral w) (fracPart False (fromIntegral w)) >>= return . Just)
       return $ case mf of
           Nothing -> Left w
           Just f -> Right f

stringLiteral   = lexeme (
                      do{ str <- between (char '"')                   
                                         (char '"' <?> "end of string")
                                         (many stringChar) 
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "literal string")

stringChar :: CharParser st (Maybe Char)
stringChar      =   do{ c <- stringLetter; return (Just c) }
                <|> try stringEscape
                <|> (char '\\' >> return (Just '\\'))
                <?> "string character"
                
stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape = do
                   char '\\'
                   choice [char 't' >> return (Just '\t'),
                           char 'n' >> return (Just '\n'),
                           char '\"' >> return (Just '\"'),
                           char '\\' >> return (Just '\\')]

--- EXPRESSION PARSING -------------------------------------------------------
--mtrace s v = return v -- trace (s ++ show v) (return v)
mtrace s v = 
    -- trace (s ++ show v) 
    (return v)

combineExprs e [] = e
combineExprs e0 ((p0,p1,op,e1):rest) = combineExprs (op p0 p1 e0 e1) rest

isFollowedBy p = do
    lookAhead p
    return ()
    
reservedOp' name = 
    lexeme $ try $
    do{ string name
      ; ( (isFollowedBy (char '-')) <|> notFollowedBy (P.opLetter lslStyle)) <?> ("end of " ++ show name)
      }

opmatch s f = (reservedOp s >> return f)
opmatch' s f = (reservedOp' s >> return f)
op0 = ((opmatch' "*" mulop) <|> (opmatch' "/" divop) <|> (opmatch' "%" modop)) <?> "operator"
op1 = ((opmatch' "+" addop) <|> (opmatch "-" subop)) <?> "operator"
op2 = ((opmatch' "<<" shiftlop) <|> (opmatch' ">>" shiftrop)) <?> "operator"
op3 = (choice [opmatch' "<" ltop, opmatch' "<=" leop, opmatch' ">" gtop, opmatch' ">=" geop]) <?> "operator"
op4 = ((opmatch' "==" eqop) <|> (opmatch' "!=" neop)) <?> "operator"

opAndExpr opf ef = do pos0 <- getPosition
                      op <- opf
                      pos1 <- getPosition
                      e <- ef
                      return (pos0,pos1,op,e)

exprChain opf ef = do e0 <- ef
                      rest <- many (opAndExpr opf ef)
                      return $ combineExprs e0 rest
                      
mulExpr = exprChain op0 expr2
addExpr = exprChain op1 mulExpr
shiftExpr = exprChain op2 addExpr
relExpr = do e0 <- shiftExpr
             rest <- (try (many (try (opAndExpr op3 shiftExpr))) <|> return [])
             return $ combineExprs e0 rest
eqExpr = exprChain op4 relExpr
bandExpr = exprChain (opmatch' "&" bandop <?> "operator") eqExpr
xorExpr = exprChain (opmatch' "^" xorop <?> "operator") bandExpr
borExpr = exprChain (opmatch' "|" borop <?> "operator") xorExpr
andExpr = exprChain (opmatch' "&&" andop <?> "operator") borExpr
orExpr = exprChain (opmatch' "||" orop <?> "operator") andExpr

bop op pos0 pos1 e0 e1 = 
    let ctx = combineContexts (srcCtx e0, pos0, pos1, srcCtx e1) in Ctx ctx $ op e0 e1 

mulop pos0 pos1 = bop Mul pos0 pos1
divop pos0 pos1 = bop Div pos0 pos1
modop pos0 pos1 = bop Mod pos0 pos1
addop pos0 pos1 = bop Add pos0 pos1
subop pos0 pos1 = bop Sub pos0 pos1
shiftlop pos0 pos1 = bop ShiftL pos0 pos1
shiftrop pos0 pos1 = bop ShiftR pos0 pos1
ltop pos0 pos1 = bop Lt pos0 pos1
leop pos0 pos1 = bop Le pos0 pos1
gtop pos0 pos1 = bop Gt pos0 pos1
geop pos0 pos1 = bop Ge pos0 pos1
eqop pos0 pos1 = bop Equal pos0 pos1
neop pos0 pos1 = bop NotEqual pos0 pos1
bandop pos0 pos1 = bop BAnd pos0 pos1
xorop pos0 pos1 = bop Xor pos0 pos1
borop pos0 pos1 = bop BOr pos0 pos1
andop pos0 pos1 = bop And pos0 pos1
orop pos0 pos1 = bop Or pos0 pos1

structAccess = do id <- ctxify identifier
                  whiteSpace
                  dot
                  whiteSpace
                  c <- oneOf "xyzs"
                  whiteSpace
                  return (id,stringToComponent [c])
var = try structAccess
   <|> do id <- ctxify identifier
          return (id,All)

assignment = ctxify $ 
             do v <- var
                op <- choice [reservedOp' "+=" >> (return $ IncBy),
                              reservedOp' "-=" >> (return $ DecBy),
                              reservedOp' "*=" >> (return $ MulBy),
                              reservedOp' "/=" >> (return $ DivBy),
                              reservedOp' "%=" >> (return $ ModBy),
                              reservedOp' "="  >> (return $ Set)] <?> "assignment operator"
                e <- expr
                return $ op v e

stringToComponent "" = All
stringToComponent "x" = X
stringToComponent "y" = Y
stringToComponent "z" = Z
stringToComponent "s" = S

listExpr = do whiteSpace
              exprs <- (brackets $ commaSep expr) <?> "list expression"
              return $ ListExpr exprs
              
-- structExpr = do  whiteSpace
--                  char '<' <?> "vector/rotation expression"
--                  whiteSpace
--                  e1 <- expr
--                  mtrace "structExpr:1 " e1
--                  char ','
--                  whiteSpace
--                  e2 <- expr
--                  mtrace "structExpr:2 " e2
--                  char ','
--                  whiteSpace
--                  e3 <- expr
--                  mtrace "structExpr:3 " e3
--                  return (e1,e2,e3)
-- vecRotExpr = do  (x,y,z) <- structExpr
--                  mtrace "vec/rot" "hi"
--                  (do char '>'
--                      whiteSpace
--                      return $ VecExpr x y z) <|> 
--                         (do char ',' 
--                             whiteSpace
--                             e <- expr 
--                             char '>'
--                             whiteSpace
--                             return $ RotExpr x y z e)

-- there's a conflict between a relational expression embedded in the last component of a vector/rotation and the
-- the normal end of the expression... in particular:
-- v = <1,1,1 > -<1,2,3>>; 
-- is not a syntax error (but is a type error), but you can't tell the difference between that and
-- v = <1,1,1 > -<1,2,3>;
-- which is the difference between two vectors (which is both syntactically correct and type correct) until you
-- parse the ';'.  So the complexity of the grammar here exists to disambiguate these cases (which requires a
-- bit of lookahead...)
structStart = do whiteSpace
                 char '<' <?> "vector/rotation expression"
                 whiteSpace
                 e1 <- expr
                 mtrace "structExpr:1 " e1
                 char ','
                 whiteSpace
                 e2 <- expr
                 mtrace "structExpr:2 " e2
                 char ',' >> whiteSpace
                 return (e1,e2)
                 
vecRotExpr = do (x,y) <- structStart
                ((try (do z <- expr
                          char ',' >> whiteSpace
                          s <- tailStruct
                          return $ RotExpr x y z s)) <|>
                 (do z <- tailStruct
                     return $ VecExpr x y z))
                     
tailStruct = (try (do e <- expr
                      char '>' >> whiteSpace
                      return e))
             <|> (do e <- shiftExpr
                     char '>' >> whiteSpace
                     return e)                 
callExpr = do 
    name <- ctxify identifier
    exprs <- parens $ commaSep expr
    return $ Call name exprs
              
castExpr = ctxify $
           do t <- parens typeName
              e <- choice [try postfixExpr, atomicExpr]
              return $ Cast t e

ctxify f = do
    pos0 <- getPosition
    v <- f
    pos1 <- getPosition
    return $ Ctx (pos2Loc (pos0,pos1)) v

notExpr = ctxify ((char '!' <?> "prefix operator") >> whiteSpace >> expr2 >>= return.Not)
invExpr = ctxify ((char '~' <?> "prefix operator") >> whiteSpace >> expr2 >>= return.Inv)
negExpr = ctxify ((char '-' <?> "prefix operator") >> whiteSpace >> expr2 >>= return.Neg)

atomicExpr = do
    aq <- getState
    (ctxify $
              try ( do m <- option 1 (reservedOp "-" >> return (-1))
                       n <- naturalOrFloat
                       return $ case n of
                           Left nat -> (IntLit $ m * fromIntegral nat)
                           Right flt -> (FloatLit $ fromRational $ toRational m * toRational flt)))
         <|> (ctxify (stringLiteral >>= return.StringLit))
         <|> (if aq then (ctxify (reservedOp "$string:" >> identifier >>= return . AQString)) else fail "")
         <|> (if aq then (ctxify (reservedOp "$integer:" >> identifier >>= return . AQInteger)) else fail "")
         <|> (if aq then (ctxify (reservedOp "$key:" >> identifier >>= return . AQKey)) else fail "")
         <|> (if aq then (ctxify (reservedOp "$float:" >> identifier >>= return . AQFloat)) else fail "")
         <|> (ctxify listExpr)
         <|> (ctxify vecRotExpr)
         <|> (ctxify $ try callExpr)
         <|> (ctxify $ do v <- var
                          return $ Get v)
         <|> parens expr

postfixExpr = ctxify $
              do v <- var
                 f <- choice [reservedOp' "++" >> return PostInc,
                              reservedOp' "--" >> return PostDec] <?> "postfix operator"
                 return $ f v
prefixExpr = ctxify $
             do f <- choice [reservedOp "--" >> return (PreDec),
                             reservedOp "++" >> return PreInc] <?> "prefix operator"
                v <- var
                return $ f v
                            
unaryExpr = choice [try prefixExpr,notExpr,invExpr, negExpr,try castExpr,atomicExpr]

expr2 :: GenParser Char Bool (Ctx Expr)
expr2 = choice [try assignment, try postfixExpr, unaryExpr]

expr1 = orExpr

expr :: GenParser Char Bool (Ctx Expr)
expr = 
    do r <- choice [try assignment, expr1]
       mtrace "expr: " r

exprParser :: String -> Either ParseError (Ctx Expr)
exprParser text = runParser expr False "" text

------------------------------------------------------------------------------
-- STATEMENT PARSING

statements = do whiteSpace
                many (ctxify statement)
             
statement = declStatement
        <|> returnStatement
        <|> jumpStatement
        <|> labelStatement
        <|> exprStatement
        <|> blockStatement
        <|> nullStatement
        <|> whileStatement
        <|> ifElseStatement
        <|> stateStatement
        <|> doWhileStatement
        <|> forStatement

declStatement = do t <- typeName
                   id <- identifier
                   rest <- (option Nothing (reservedOp' "=" >> expr >>= return . Just))
                   semi
                   return $ Decl (Var id t) rest

integerType     = (reserved "integer" >> return LLInteger)
floatType       = (reserved "float" >> return LLFloat)
keyType         = (reserved "key" >> return LLKey)
vectorType      = (reserved "vector" >> return LLVector)
stringType      = (reserved "string" >> return LLString)
rotationType    = ((reserved "rotation"<|>reserved "quaternion") >> return LLRot)
listType        = (reserved "list" >> return LLList)

typeName = choice [integerType,floatType,keyType,vectorType,stringType,rotationType,listType]

returnStatement = do  reserved "return"
                      e <- (option Nothing (expr >>= return . Just))
                      semi
                      return $ Return e
jumpStatement = do reserved "jump"
                   id <- identifier
                   semi
                   return $ Jump id
labelStatement = do reservedOp "@"
                    id <- identifier
                    semi
                    return $ Label id
exprStatement = do e <- expr
                   semi
                   return $ Do e

blockStatement = do stmts <- braces statements
                    return $ Compound stmts

nullStatement = (semi >> return NullStmt)

whileStatement = do reserved "while"
                    e <- parens expr
                    stmt <- statement
                    return $ While e stmt
ifElseStatement = do reserved "if"
                     e <- parens expr
                     stmt1 <- statement
                     stmt2 <- (option NullStmt (reserved "else" >> statement))
                     return $ If e stmt1 stmt2
                     
stateStatement = do reserved "state"
                    id <- (identifier <|> (reserved "default" >> return "default"))
                    semi
                    return $ StateChange id

forStatement = do reserved "for"
                  char '('
                  whiteSpace
                  mexpr1 <- commaSep expr
                  semi
                  mexpr2 <- option Nothing (expr >>= return . Just)
                  semi
                  mexpr3 <- commaSep expr
                  char ')'
                  whiteSpace
                  stmt <- statement
                  return $ For mexpr1 mexpr2 mexpr3 stmt

doWhileStatement = do reserved "do"
                      stmt <- statement
                      reserved "while"
                      e <- parens expr
                      return $ DoWhile stmt e

parseType text = runParser typeName False "" text
------------------------------------------------------------
-- HANDLER Parsing

hparam theType = ctxify $
                 do t <- (typeParser theType)
                    id <- identifier
                    return $ Var id t
                    
typeParser LLInteger = integerType
typeParser LLFloat = floatType
typeParser LLList = listType
typeParser LLString = stringType
typeParser LLVector = vectorType
typeParser LLKey = keyType
typeParser LLRot = rotationType

hparams [] = return []
hparams (theType:[]) = (hparam theType >>= return . (:[]))
hparams (theType:ts) = do p <- hparam theType
                          comma
                          ps <- hparams ts
                          return (p:ps)

handlerName name = try (do id <- symbol name; notFollowedBy identLetter; return id)

handler' name types = do ctxname <- ctxify $ handlerName name
                         parms <- parens (hparams types)
                         stmts <- braces statements
                         return $ Handler ctxname parms stmts
 
handler = choice $ map (\ (n,ts) -> handler' n ts) goodHandlers

-------------------------------------------------------------
-- STATE parsing

stateName = choice [reserved "default" >> return "default" , reserved "state" >> identifier >>= return]

stateDecl = do name <- ctxify stateName
               handlers <- braces $ many handler
               return $ State name handlers

stateDecls = many stateDecl

--------------------------------------------------------------
varOrFunc =   do (Ctx ctx (t,id)) <- ctxify $ do
                     t <- try typeName
                     id <- ctxify identifier <?> "identifier"
                     return (t,id)
                 choice [func t id, gvar ctx t id]
          <|> do id <- ctxify identifier <?> "identifier"
                 func LLVoid id
func t id = do ps <- parens params
               stmts <- braces statements
               return $ GF $ Func (FuncDec id t ps) stmts
gvar ctx t (Ctx _ id) = do mexpr <- option Nothing (reservedOp' "=" >> expr >>= return . Just)
                           semi
                           return $ GV (Ctx ctx (Var id t)) mexpr
--------------------------------------------------------------
-- FUNCTION parsing

param = ctxify $
        do t <- typeName
           id <- identifier
           return $ Var id t
params = commaSep param

-- function = do (t,id,ps) <- try $ do t <- option LLVoid typeName <?> "type name"
--                                     id <- ctxify identifier <?> "function name"
--                                     ps <- parens params
--                                     return (t,id,ps)
--               stmts <- braces statements
--               return $ GF $ Func (FuncDec id t ps) stmts
---------------------------------------------------------------
-- GLOBAL VARIABLES parsing

-- globals allow no initialization or initialization by 'constant' expressions... 
-- we can allow any expressions though and have semantic analysis catch problems

-- globvar = do var <- ctxify $ do
--                  t <- typeName <?> "type name"
--                  id <- identifier
--                  return (Var id t)
--              mexpr <- option Nothing (reservedOp "=" >> expr >>= return.Just)
--              semi
--              return $ GV var mexpr
----------------------------------------------------------------
-- IMPORT (meta-lsl directive) parsing

gimport = do reserved "$import" <?> "$import keyword"
             ids <- (ctxify $ (char '$' >> identifier >>= return . (:[]) . ("$"++)))
                 <|>(ctxify $ sepBy identifier dot)
             let id = fmap (concat . intersperse ".") ids
             let binding = do id0 <- identifier
                              reservedOp "="
                              id1 <- identifier
                              return (id0,id1)
             bindings <- option [] $ parens $ commaSep binding
             prefix <- option "" identifier
             semi
             return $ GI id bindings prefix
----------------------------------------------------------------
-- all globals parsing             

globals = many $ choice [gimport,varOrFunc]

lslParser = do whiteSpace
               globs <- globals
               ss <- stateDecls
               eof
               return $ LSLScript globs ss

moduleParser = do whiteSpace
                  reserved "$module"
                  freevars <- option [] $ parens params
                  globs <- globals
                  eof
                  return $ LModule globs freevars

parseFromString parser string =
    case runParser parser False "" string of
        Left err -> Left (snd (fromParseError err))
        Right x -> Right x

-- | Parse an LSL script into its syntax tree.
parseScriptFromString :: String -> Either String LSLScript
parseScriptFromString s = parseFromString lslParser s
-- | Parse an LSL (Plus) module into its syntax tree.
parseModuleFromString :: String -> Either String LModule
parseModuleFromString s = parseFromString moduleParser s

-- | Parse an LSL script, with possible antiquotations, into its syntax tree.
parseScriptFromStringAQ :: String -> Either ParseError LSLScript
parseScriptFromStringAQ s = runParser lslParser True "" s
-- | Parse an LSL (Plus) module, with possible antiquotations, into its syntax tree.
parseModuleFromStringAQ :: String -> Either ParseError LModule
parseModuleFromStringAQ s = runParser moduleParser True "" s

parseModule :: (MonadIO m) => SourceName -> m (Either (SourceContext,String) LModule)
parseModule file = parseFile moduleParser file
parseScript file = parseFile lslParser file

fromParseError :: ParseError -> (SourceContext,String)
fromParseError err =
        let pos = errorPos err
            msg = showErrorMessages "or" "unknown parse error" 
                                    "expecting" "unexpected" "end of input" (errorMessages err)
        in (TextLocation { textLine0 = sourceLine pos, textColumn0 = sourceColumn pos,
                           textLine1 = sourceLine pos, textColumn1 = sourceColumn pos,
                           textName = sourceName pos },
            msg)

parseFile p file =
    do s <- liftIO $ readFile file
       case parser s of
           Left err -> return $ Left (fromParseError err)
           Right x -> return $ Right x
    where parser = runParser p False file
    
txtLoc pos len =
    TextLocation { textName = sourceName pos, 
                   textColumn0 = sourceColumn pos, 
                   textLine0 = sourceLine pos,
                   textColumn1 = sourceColumn pos + (len-1),
                   textLine1 = sourceLine pos }
    
idLoc pos id = txtLoc pos $ length id

pos2Loc (pos0,pos1) = 
     TextLocation { 
         textName = sourceName pos0,
         textColumn0 = sourceColumn pos0,
         textLine0 = sourceLine pos0,
         textColumn1 = sourceColumn pos1,
         textLine1 = sourceLine pos1
     }
     
combineContexts (UnknownSourceContext,pos0,pos1,UnknownSourceContext) = pos2Loc (pos0,pos1)
combineContexts (TextLocation l0 c0 l1 c1 n,_,_,TextLocation l0' c0' l1' c1' n') =
    TextLocation l0 c0 l1' c1' n
combineContexts (TextLocation l0 c0 l1 c1 n,_,pos,_) =
    TextLocation l0 c0 (sourceLine pos) (sourceColumn pos) n
combineContexts (_,pos,_,TextLocation l0 c0 l1 c1 n) =
    TextLocation (sourceLine pos) (sourceColumn pos) l1 c1 n

