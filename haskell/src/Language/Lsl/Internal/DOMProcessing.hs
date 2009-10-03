{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances,
    GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.DOMProcessing(
    Content(..),
    Document(..),
    Element(..),
    Posn,
    xmlParse,
    ElementAcceptContext(..),
    getContent,setContent,
    getAttrs,setAttrs,
    getTag,
    tag, tagit, clattr, clsit,
    req, opt, def, text, val,
    choice, choicec, choicet, choose,
    elist,liste,SimpleContext(..),bool,
    runSimpleContext
    ) where

import Data.Maybe
import Control.Applicative
import Control.Monad(liftM,unless,foldM,ap)
import Control.Monad.Error(MonadError(..),ErrorT(..))
import Control.Monad.State(MonadState(..),State(..),evalState)
import Language.Lsl.Internal.Util(readM)
import Text.XML.HaXml(Attribute,AttValue(..),Document(..),Element(..),
    Content(..),Reference(..),xmlParse)
import Text.XML.HaXml.Posn(Posn(..))

class (MonadError String m, Applicative m) => ElementAcceptContext m where
    getContext :: m (Element Posn)
    setContext :: Element Posn -> m ()
    withContext :: Element Posn -> m a -> m a
   
setContent :: ElementAcceptContext m => [Content Posn] -> m ()
setContent c = getContext >>= \ (Elem nm attr _) -> setContext (Elem nm attr c)

setAttrs :: ElementAcceptContext m => [Attribute] -> m ()
setAttrs a = getContext >>= \ (Elem nm _ c) -> setContext (Elem nm a c)

getContent :: ElementAcceptContext m => m [Content Posn]
getContent = getContext >>= \ (Elem _ _ c) -> return c

getTag :: ElementAcceptContext m => m String
getTag = getContext >>= \ (Elem tag _ _) -> return tag

getAttrs :: ElementAcceptContext m => m [Attribute]
getAttrs = getContext >>= \ (Elem _ attrs _) -> return attrs

type CAcceptor m t = m t

runAcc :: ElementAcceptContext m => m b -> (a -> m b) -> CAcceptor m (Maybe a) -> m b
runAcc nf f acceptor = do 
    content <- getContent
    find [] content `catchError`
        \ s -> getTag >>= \ t -> throwError ("in element " ++ t ++ ": " ++ s)
    where find bs [] = nf
          find bs (ce@(CElem e@(Elem nm attrs ec) i):cs) = 
              (try nm e i >>= maybe (find (ce:bs) cs) (\ v -> setContent (reverse bs ++ cs) >> f v))
          find bs (ce:cs) = find (ce:bs) cs
          try nm e i = withContext e acceptor `catchError` \ s ->
              throwError ("in element " ++ nm ++ " in " ++ show i ++ ": " ++ s)

tag :: ElementAcceptContext m => String -> CAcceptor m ()
tag t = do
    tagMatches <- (==) <$> pure t <*> getTag
    unless tagMatches $ throwError ("tag " ++ t ++ " not matched")

tagit :: ElementAcceptContext m => String -> CAcceptor m a 
    -> CAcceptor m (Maybe a)
tagit s acc = do
    found <- (tag s >> return True) `catchError` const (return False)
    if found then Just <$> acc else return Nothing

clattr :: ElementAcceptContext m => String -> CAcceptor m ()
clattr c = do
    attrs <- getAttrs
    unless (("class",AttValue [Left c]) `elem` attrs) $
        throwError ("attribute class=\"" ++ c ++ "\" not matched")
        
clsit :: ElementAcceptContext m => String -> CAcceptor m a 
    -> CAcceptor m (Maybe a)
clsit s acc = 
    (clattr s >> Just <$> acc) `catchError` const (return Nothing)

-- req : this element is a required child, with the specified tag
req t acceptor = runAcc (throwError ("element not found: " ++ t)) return 
    (tagit t acceptor)

opt t acceptor = runAcc (return Nothing) (return . Just) 
    (tagit t acceptor)
def t def acceptor = runAcc (return def) return (tagit t acceptor)

text :: (ElementAcceptContext m) => m String
text = do
    s <- (liftM concat . mapM textItem) =<< getContent
    setContent []
    return s

val :: (ElementAcceptContext m, Read a) => CAcceptor m a
val = text >>= readM

bool = text >>= \ s -> case s of
    "true" -> return True
    "false" -> return False
    "True" -> return True
    "False" -> return False
    s -> throwError ("unacceptable bool value: " ++ s)
    
textItem :: ElementAcceptContext m => Content Posn -> m String
textItem (CElem _ i) = throwError ("unexpected element in " ++ show i)
textItem (CString _ s _) = return s
textItem (CRef (RefEntity "lt") _) = return "<"
textItem (CRef (RefEntity "gt") _) = return ">"
textItem (CRef (RefEntity "amp") _) = return "&"
textItem (CRef (RefEntity "quot") _) = return "\""
textItem (CRef (RefEntity "apos") _) = return "'"
textItem (CMisc _ i) = throwError ("unexpected content in " ++ show i)

choose :: ElementAcceptContext m => CAcceptor m a -> CAcceptor m a -> CAcceptor m a
choose i j = i `catchError` (const j)

choice :: ElementAcceptContext m => [CAcceptor m (Maybe a)] -> CAcceptor m (Maybe a)
choice [] = return Nothing --throwError "nothing matched"
choice (c:cs) = c >>= maybe (choice cs) (return . Just)

choicec :: ElementAcceptContext m => [(String,CAcceptor m a)] -> CAcceptor m (Maybe a)
choicec = choice . map (uncurry clsit)

choicet :: ElementAcceptContext m => [(String,CAcceptor m a)] -> CAcceptor m (Maybe a)
choicet = choice . map (uncurry tagit)

elist :: ElementAcceptContext m => CAcceptor m (Maybe a) -> CAcceptor m [a]
elist acc = foldM f [] =<< getContent where
    f l (CElem e@(Elem nm _ _) i) = --withContext e acc >>= return . (l ++) . (:[])
        withContext e acc >>= 
            maybe (throwError ("unexpected element " ++ nm ++ " in list in " ++ show i))
            (return . (l ++) . (:[]))
    f l (CMisc _ i) = err i
    f l (CRef _ i) = err i
    f l (CString _ _ i) = return l
    err i = throwError ("in " ++ show i ++ " unexpected non-element content")
          
liste s = (elist . tagit s)

newtype SimpleContext a = SimpleContext { unSimpleContext :: ErrorT String (State (Element Posn)) a }
    deriving (Monad)

instance MonadState (Element Posn) SimpleContext where
   get = SimpleContext { unSimpleContext = get }
   put v = SimpleContext { unSimpleContext = put v }
   
instance MonadError String SimpleContext where
    throwError e = SimpleContext { unSimpleContext = throwError e }
    catchError v f = SimpleContext { unSimpleContext = catchError (unSimpleContext v) (unSimpleContext . f) }

instance Functor SimpleContext where
    fmap = liftM
    
instance Applicative SimpleContext where
   pure  = return
   (<*>) = ap

instance ElementAcceptContext SimpleContext where
   getContext = get
   setContext = put
   withContext c a = either throwError return $ runSimpleContext a c
   
runSimpleContext = (evalState . runErrorT . unSimpleContext)
