module Language.Lsl.Internal.DOMProcessing(ElemAcceptor(..),
                         findElement,
                         findOptionalElement,
                         findOrDefault,
                         findSimple,
                         findSimpleOrDefault,
                         findValueOrDefault,
                         findBoolOrDefault,
                         findValue,
                         findOptionalValue,
                         findOptionalBool,
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
                         Content(..),
                         Document(..),
                         Element(..),
                         xmlParse,
                         --module Text.XML.HaXml,
                         --module Text.XML.HaXml.Posn
                         ) where

import Data.Maybe
import Control.Monad(MonadPlus(..),liftM2)
import Control.Monad.Error(MonadError(..))
import Language.Lsl.Internal.Util(readM)
import Text.XML.HaXml(Attribute,AttValue(..),Document(..),Element(..),Content(..),Reference(..),xmlParse)
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

findOptionalValue name contents =
    do (v,rest) <- findOptionalElement (simpleElement name) contents
       case v of
           Nothing -> return (Nothing,contents)
           Just s -> do
               value <- readM s
               return (value, rest)
               
findOptionalBool name contents =
    do (v,rest) <- findOptionalElement (simpleElement name) contents
       case v of 
           Nothing -> return (Nothing,contents)
           Just "true" -> return (Just True,rest)
           Just "false" -> return (Just False,rest)
           Just s -> fail ("unable to parse " ++ s)
           
findBoolOrDefault def name contents =
    do (sval,rest) <- findOptionalElement (simpleElement name) contents
       case sval of 
            Nothing -> return (def,contents)
            Just "true" -> return (True,rest)
            Just "false" -> return (False,rest)
            Just s -> fail ("unable to parse " ++ s)
            
findValueOrDefault def name contents =
    do (sval,rest) <- findOptionalElement (simpleElement name) contents
       case sval of
           Nothing -> return (def, contents)
           Just s -> do
               value <- readM s
               return (value, rest)
                     
-- for backwards compatibility
attrString (AttValue v) = concatMap decode v
    where
      decode (Left  v)               = v
      decode (Right (RefEntity ent)) = "&"++ent++";"
      decode (Right (RefChar cref))  = "&"++show cref++";"

-- type ContentAcceptor a = [Content Posn] -> Either String a
-- type ContentFinder a = [Content Posn] -> Either String (Maybe a, [Content Posn])
-- type ElementAcceptor a = Element Posn -> Either String a
-- type ElementTester a = Element Posn -> Either String (Maybe a)
-- type AttributeAcceptor a = [Attribute] -> Either String a

-- mustBe :: String -> (b -> a) -> ContentAcceptor b -> ElementAcceptor a
-- mustBe tag f cf (Elem name _ cs) | tag /= name = Left ("expected " ++ tag)
--                                  | otherwise = case cf cs of
--                                       Left s -> Left s
--                                       Right v -> Right (f v)

-- canBe :: String -> (b -> a) -> ContentAcceptor b -> ElementTester a
-- canBe tag f cf (Elem name _ cs) | tag /= name = Right Nothing
--                                 | otherwise = case cf cs of
--                                       Left s -> Left s
--                                       Right v -> Right (Just (f v))

-- mustBeWith :: String -> (a -> b -> c) -> AttributeAcceptor a -> ContentAcceptor b -> ElementAcceptor c
-- mustBeWith tag f af cf (Elem name attrs cs) | tag /= name = Left ("expected " ++ tag)
--                                             | otherwise = do
--                                                   av <- af attrs
--                                                   cv <- cf cs
--                                                   return (f av cv)

-- canBeWith :: String -> (a -> b -> c) -> AttributeAcceptor a -> ContentAcceptor b -> ElementTester c
-- canBeWith tag f af cf (Elem name attrs cs) | tag /= name = Right Nothing
--                                            | otherwise = do
--                                                   av <- af attrs
--                                                   cv <- cf cs
--                                                   return (Just (f av cv))

-- liftElemTester :: ((Element Posn) -> Either String (Maybe a)) -> (Content Posn -> Either String (Maybe a))
-- liftElemAcceptor ef (CElem e pos) = case ef e of
--     Left s -> Left ("at " ++ show pos ++ ": " ++ s)
--     Right v -> Right v
--     
-- canHaveElem :: ElementAcceptor a -> ContentFinder a
-- canHaveElem ef cs = mapM (\ c -> liftElemAcceptor ef c >>= return . (,) c) cs >>= (\ vs -> case span (isNothing . snd) vs of
--     (bs,[]) -> Right (Nothing,map fst bs)
--     (bs,c:cs) -> Right (snd c,map fst $ bs ++ cs))
--    
-- mustHaveElem :: ElementAcceptor a -> ContentFinder a
-- mustHaveElem ef cs = mapM (\ c -> liftElemAcceptor ef c >>= return . (,) c) cs >>= (\ vs -> case span (isNothing . snd) vs of
--     (bs,[]) -> Left ("element not found")
--     (bs,c:cs) -> Right (snd c,map fst $ bs ++ cs))

-- boolContent cs = simpleContent cs >>= (\ v -> case v of
--     "true" -> Right True
--     "false" -> Right False
--     s -> Left ("unrecognized bool " ++ s))
--     
-- readableContent :: Read a => ContentAcceptor a
-- readableContent cs = simpleContent cs >>= readM

-- simpleContent :: ContentAcceptor String
-- simpleContent cs = mapM processContentItem cs >>= return . concat
--     where
--         processContentItem (CElem (Elem name _ _) _) = Left ("unexpected content element (" ++ name ++ ")")
--         processContentItem (CString _ s _) = Right s
--         processContentItem (CRef (RefEntity "lt") _) = Right "<"
--         processContentItem (CRef (RefEntity "gt") _) = Right ">"
--         processContentItem (CRef (RefEntity "amp") _) = Right "&"
--         processContentItem (CRef (RefEntity "quot") _) = Right "\""
--         processContentItem (CRef (RefEntity "apos") _) = Right "'"
--         processContentItem (CMisc _ _) = Right "unexpected content"

-- empty :: ContentAcceptor ()
-- empty [] = Right ()
-- empty _ = Left "unexpected content"

-- data FooBar = FooBar { foo :: Int, bar :: String }

-- fooE :: ElementAcceptor Int
-- fooE = mustBe "foo" id readableContent

-- -- foobar = mustBe "FooBar" (\ _ -> FooBar 0 "") ( \ cs -> do
--      mustHave (mustBe "foo" 