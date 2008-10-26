module Lsl.UnitTestParser(parseUnitTestFile,parseUnitTestInput) where

import Lsl.Type
import Lsl.UnitTest
import Control.Monad.Error
import IO hiding (try)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Text.ParserCombinators.Parsec.Expr

-- define basic rules for lexical analysis
lslStyle = javaStyle
             { P.reservedOpNames= ["<", ">", "-", ":", "->", "=", "()"],
               P.reservedNames = [],
               P.caseSensitive = True }
lexer :: P.TokenParser ()
lexer  = P.makeTokenParser lslStyle

identLetter = P.identLetter lslStyle
               
identifier       = P.identifier lexer
reserved         = P.reserved lexer
operator         = P.operator lexer
reservedOp       = P.reservedOp lexer
                        
charLiteral      = P.charLiteral lexer
stringLiteral    = P.stringLiteral lexer
natural          = P.natural lexer
integer          = P.integer lexer
float            = P.float lexer
naturalOrFloat   = P.naturalOrFloat lexer
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

boolLiteral = choice [reserved "true" >> return True,
                      reserved "false" >> return False]

unitTest = do reserved "test"
              (ep,args,ret) <- moduleEntryPoint <|> scriptEntryPoint
              init <- option [] (reserved "init" >> parens bindings)
              (mode,callsList) <- do
                  reserved "calls"
                  mode <- choice [reserved "strict" >> return Strict,
                                  reserved "normal" >> return Normal,
                                  reserved "exhaust" >> return Exhaust,
                                  reserved "nice" >> return Nice]
                  f <- option ([]++) (braces $ calls mode)
                  return (mode,f [])
              after <- option [] (reserved "after" >> parens bindings)
              newState <- option Nothing (reserved "new_state" >> identifier >>= return . Just)
              return $ LSLUnitTest {
                  unitTestName = "",
                  entryPoint = ep,
                  initialGlobs = init,
                  arguments = args,
                  expectedCalls = FuncCallExpectations mode callsList,
                  expectedReturn = ret,
                  expectedGlobalVals = after,
                  expectedNewState = newState }
                  
unitTests = endBy unitTest semi
              
moduleEntryPoint = do
    reserved "module"
    mid <- stringLiteral
    funcCall (ModuleFunc mid)
    
scriptEntryPoint = do
    reserved "script"
    sid <- stringLiteral
    choice [try $ funcCall (ScriptFunc sid),handlerCall (ScriptHandler sid)]
    
funcCall epCons = do
    fname <- identifier
    args <- params
    ret <- option Nothing (reservedOp "->" >> litPlus >>= return . Just)
    return (epCons fname, args, ret)
    
handlerCall epCons = do
    sname <- identifier
    reservedOp "."
    hname <- identifier
    args <- params
    return (epCons sname hname, args, Nothing)
    
structLit = do  reservedOp "<"
                v1 <- signedFloat float1
                comma
                v2 <- signedFloat float1
                comma
                v3 <- signedFloat float1
                return (v1,v2,v3)

optNeg = option 1 (reservedOp "-" >> return (-1))

vecRotLit = do (x,y,z) <- structLit
               (do char '>'
                   return $ VVal x y z) <|> 
                        (do comma 
                            s <- signedFloat float1
                            reservedOp ">"
                            return $ RVal x y z s)

signedFloat :: Real a => GenParser Char () a -> GenParser Char () Float
signedFloat p = do 
    n <- optNeg
    v <- p
    return (fromInteger n * (fromRational $ toRational v))

float1 :: GenParser Char () Float
float1 = do v <- naturalOrFloat
            return $ case v of
                Left i -> fromInteger i
                Right d -> fromRational $ toRational d

floatLit = do f <- signedFloat float
              return $ FVal f
              
intLit = do n <- option (1) (reservedOp "-" >> return (-1))
            v <- natural
            return $ IVal $ fromInteger (n*v)
            
stringLit = do s <- stringLiteral
               return $ SVal s
               
keyLit = do reserved "key"
            s <- stringLiteral
            return $ KVal s
            
listLit = do l <- brackets $ sepBy simpleLiteral comma
             return $ LVal l

simpleLiteralParsers = [stringLit,try floatLit,intLit,keyLit,vecRotLit]
simpleLiteral = choice simpleLiteralParsers

literal = choice (listLit:simpleLiteralParsers)
litPlus = choice (voidLit:listLit:simpleLiteralParsers)

voidLit = do reservedOp "()" >> return VoidVal

params = parens $ sepBy literal comma

dontCare = do reservedOp "_"
              return Nothing

maybeParam = dontCare <|> do lit <- literal
                             return $ Just lit
                             
maybeParams = parens $ sepBy maybeParam comma

callCount = braces $ natural

call mode = 
    do
        flist <- parens (calls mode)
        count <- option 1 callCount
        return $ concat . replicate (fromInteger count) . flist
    <|>
    do
        name <- identifier
        args <- maybeParams
        ret <- option VoidVal (reservedOp "->" >> literal)
        return ([((name,args),ret)]++)
     
calls mode = do callsList <- sepEndBy (call mode) semi
                return $ foldl (.) ([]++) callsList

binding = do name <- identifier
             reservedOp "="
             v <- literal
             return (name,v)
             
bindings = sepBy binding comma

parseUnitTestFile file = readFile file >>= checkTests file
--     do s <- readFile file
--        case parse unitTests file s of
--            Left err -> fail (show err)
--            Right us -> return us

parseUnitTestInput handle = hGetContents handle >>= checkTests ""
--     do s <- hGetContents handle
--        case parse unitTests "" s of
--            Left err -> fail (show err)
--            Right us -> return us
           
checkTests inputName input =
    case parse unitTests inputName input of
        Left err -> fail (show err)
        Right us -> return us