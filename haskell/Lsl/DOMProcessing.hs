module Lsl.DOMProcessing(ElemAcceptor(..),
                         findElement,
                         findOptionalElement,
                         ctxelem,
                         simple,
                         matchChoice,
                         matchMaybe,
                         cmatch,
                         match,
                         attValueString,
                         elementList,
                         elementsOnly) where

import Control.Monad
import Control.Monad.Error
import Data.List
import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty

data Monad m => ElemAcceptor m t = ElemAcceptor { acceptorTag :: String,  acceptorFunc :: (Element Posn -> m t) }

findElement (ElemAcceptor tag acceptor) list =          
    let find' _ [] = fail ("element not found: " ++ tag)
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
                                                                
ctxelem f (CElem e i) =
    case f e of
        Left s -> fail ("at " ++ (show i) ++ ": " ++ s)
        Right v -> return v

cmatch acceptor (CElem e@(Elem n _ _) i) =
    case match acceptor e of
        Left s -> fail ("at " ++ (show i) ++ ": " ++ s)
        Right v -> return v

match acceptor e@(Elem n _ _) | n /= acceptorTag acceptor = fail ("unexpected element " ++ n ++ " (expected " ++ acceptorTag acceptor ++ ")")
                              | otherwise = acceptorFunc acceptor e
                              
simple (Elem _ _ []) = return ""
simple (Elem _ _ contents) = return (processContents contents)
--simple _ = fail "unexpected content in tag"

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
            Nothing -> fail ("at " ++ show i ++ ": unexpected element " ++ name)
            Just val -> return val

matchMaybe :: Monad m => Element Posn -> ElemAcceptor (Either String) t -> m (Maybe t)
matchMaybe e@(Elem n a c) (ElemAcceptor name f) | n /= name = return Nothing
                                                | otherwise = case f e of
                                                                  Left s -> fail s
                                                                  Right v -> return $ Just v

attValueString (AttValue list) = concat [ s | Left s <- list ]

elementList name itemAcceptor =
    let f (Elem _ _ contents) = mapM (cmatch itemAcceptor) [ e | e@(CElem _ _) <- contents ]
    in ElemAcceptor name f
    
elementsOnly cs = [ e | e@(CElem _ _) <- cs]