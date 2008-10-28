module Lsl.DOMProcessing(ElemAcceptor(..),
                         findElement,
                         findOptionalElement,
                         findOrDefault,
                         findSimple,
                         findSimpleOrDefault,
                         findValueOrDefault,
                         findValue,
                         valueAcceptor, -- String -> ElemAcceptor m t
                         ctxelem,
                         simple,
                         simpleElement,
                         matchChoice,
                         matchMaybe,
                         cmatch,
                         match,
                         attValueString,
                         acceptList, -- 
                         elementList,
                         elementListWith,
                         elementsOnly,
                         attrString,
                         module Text.XML.HaXml,
                         module Text.XML.HaXml.Posn) where

import Control.Monad(MonadPlus(..),liftM2)
import Control.Monad.Error(MonadError(..))
import Lsl.Util(readM)
import Text.XML.HaXml(AttValue(..),Document(..),Element(..),Content(..),Reference(..),xmlParse)
import Text.XML.HaXml.Posn(Posn(..))

data Monad m => ElemAcceptor m t = ElemAcceptor { acceptorTag :: String,  acceptorFunc :: (Element Posn -> m t) }

findElement (ElemAcceptor tag acceptor) list =          
    let find' _ [] = throwError ("element not found: " ++ tag)
        find' bs (ce@(CElem (Elem nm _ _) _):cs) | nm == tag  = 
                                                       do  val <- ctxelem acceptor ce
                                                           return (val, reverse bs ++ cs)
                                                 | otherwise = find' (ce:bs) cs
    in find' [] list

findOptionalElement (ElemAcceptor tag acceptor) list =
    let find' bs [] = return (Nothing,bs)
        find' bs (ce@(CElem (Elem nm _ _) _):cs) | nm == tag =
                                                     do val <- ctxelem acceptor ce
                                                        return (Just val, reverse bs ++ cs)
                                                 | otherwise = find' (ce:bs) cs
    in find' [] list

findOrDefault def acceptor list =
    do (v,rest) <- findOptionalElement acceptor list
       return $ case v of Nothing -> (def, rest); Just v' -> (v',rest)

findSimpleOrDefault def tag = findOrDefault def (simpleElement tag)
                                                                     
ctxelem f (CElem e i) = catchError (f e) handler
    where handler s = throwError ("at " ++ (show i) ++ ": " ++ s)

cmatch acceptor (CElem e@(Elem n _ _) i) = catchError (match acceptor e) handler
    where handler s = throwError ("at " ++ (show i) ++ ": " ++ s)

match acceptor e@(Elem n _ _) | n /= acceptorTag acceptor = throwError ("unexpected element " ++ n ++ " (expected " ++ acceptorTag acceptor ++ ")")
                              | otherwise = acceptorFunc acceptor e
                              
simple (Elem _ _ []) = return ""
simple (Elem _ _ contents) = return (processContents contents)

simpleElement s = ElemAcceptor s simple

findSimple s = findElement (simpleElement s)

processContents = concatMap processContentItem
processContentItem (CElem (Elem name _ _) _) = error ("unexpected content element (" ++ name ++ ")")
processContentItem (CString _ s _) = s
processContentItem (CRef (RefEntity "lt") _) = "<"
processContentItem (CRef (RefEntity "gt") _) = ">"
processContentItem (CRef (RefEntity "amp") _) = "&"
processContentItem (CRef (RefEntity "quot") _) = "\""
processContentItem (CRef (RefEntity "apos") _) = "'"
processContentItem (CMisc _ _) = error "unexpected content"

matchChoice acceptors (CElem e@(Elem name _ _) i) =
    do  result <- foldl (liftM2 mplus) (return Nothing) $ map (matchMaybe e) acceptors
        case result of
            Nothing -> throwError ("at " ++ show i ++ ": unexpected element " ++ name)
            Just val -> return val

matchMaybe e@(Elem n a c) (ElemAcceptor name f) | n /= name = return Nothing
                                                | otherwise = f e >>= return . Just

attValueString (AttValue list) = concat [ s | Left s <- list ]

acceptList itemAcceptor (Elem _ _ contents) = mapM (cmatch itemAcceptor) (elementsOnly contents)

elementList name itemAcceptor = ElemAcceptor name (acceptList itemAcceptor)

acceptListWith f (Elem _ _ contents) = mapM f (elementsOnly contents)
elementListWith name f = ElemAcceptor name (acceptListWith f)
    
elementsOnly cs = [ e | e@(CElem _ _) <- cs]

acceptValue e = do
   s <- simple e
   value <- readM s
   return value
   
valueAcceptor s = ElemAcceptor s acceptValue

findValue name contents =
    do (sval,rest) <- findSimple name contents
       value <- readM sval
       return (value,rest)

findValueOrDefault def name contents =
    do (sval,rest) <- findOptionalElement (simpleElement name) contents
       case sval of
           Nothing -> return (def, contents)
           Just s -> do
               value <- readM s
               return (value, contents)
                     
-- for backwards compatibility
attrString (AttValue v) = concatMap decode v
    where
      decode (Left  v)               = v
      decode (Right (RefEntity ent)) = "&"++ent++";"
      decode (Right (RefChar cref))  = "&"++show cref++";"
