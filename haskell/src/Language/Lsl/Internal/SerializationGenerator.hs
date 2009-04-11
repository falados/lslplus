{-# OPTIONS_GHC -XTemplateHaskell -XFlexibleInstances -XTypeSynonymInstances -XScopedTypeVariables 
                -XOverlappingInstances
  #-}
module Language.Lsl.Internal.SerializationGenerator where

import Language.Haskell.TH
import Control.Monad
import qualified Control.Monad.State as State
import qualified Data.Map as M
import Language.Lsl.Internal.XmlCreate
import Language.Lsl.Internal.DOMProcessing
import Language.Lsl.Internal.DOMCombinators
import Data.Generics
import Data.List
import Debug.Trace
import Text.XML.HaXml.Parse hiding (fst3,snd3,thd3)
import Text.XML.HaXml.Posn

compose = '(.)
concatNM = '(++)
concatV = VarE concatNM
concatE = foldl1 (\ l r -> AppE (AppE concatV l) r)
concatV' = varE concatNM

stringE' = (LitE . StringL)

jrepSimpleNameE = VarE 'jrepSimpleName
jrepTypeGenE = VarE 'jrepTypeGen

data JRep = JRep { jrepClassGen :: (Maybe (String -> [(String,String)])),
                   jrepTypeGen :: (String -> String),
                   jrepSimpleName :: String }

class JavaRep a where
    representation :: a -> JRep
    representative :: a
    xmlSerialize :: Maybe String -> a -> String -> String
    xmlDefaultTag :: a -> String
    subElemDescriptor :: (a -> b) -> String -> ElementTester b
    elemDescriptor :: (a -> b) -> ElementTester b
    contentFinder :: (a -> b) -> String -> ContentFinder b
        
instance JavaRep Int where
    representation _ = JRep Nothing (const "int") "int"
    representative = (0 :: Int)
    xmlSerialize t i = emit (maybe "int" id t) [] [shows i]   
    xmlDefaultTag _ = "int"
    subElemDescriptor f tag = el tag f readableContent
    elemDescriptor f = el "int" f readableContent
    contentFinder f tag = mustHaveElem $ subElemDescriptor f tag
    
instance JavaRep Float where
    representation _ = JRep Nothing (const "float") "float"
    representative = (0 :: Float)
    xmlSerialize t i = emit (maybe "float" id t) [] [shows i]   
    xmlDefaultTag _ = "float"
    subElemDescriptor f tag = el tag f readableContent
    elemDescriptor f = el "float" f readableContent
    contentFinder f tag = mustHaveElem $ subElemDescriptor f tag
    
instance JavaRep Double where
    representation _ = JRep Nothing (const "double") "double"
    representative = (0 :: Double)
    xmlSerialize t i = emit (maybe "double" id t) [] [shows i]   
    xmlDefaultTag _ = "double"
    subElemDescriptor f tag = el tag f readableContent
    elemDescriptor f = el "double" f readableContent
    contentFinder f tag = mustHaveElem $ subElemDescriptor f tag

instance JavaRep Char where
    representation _ = JRep Nothing (const "char") "char"
    representative = (' ')
    xmlSerialize t c = emit (maybe "char" id t) [] [shows c]
    xmlDefaultTag _ = "char"
    subElemDescriptor f tag = el tag f readableContent
    elemDescriptor f = el "char" f readableContent
    contentFinder f tag = mustHaveElem $ subElemDescriptor f tag
        
instance JavaRep Bool where
    representation _ = JRep Nothing (const "boolean") "boolean"
    representative = True
    xmlSerialize t b = emit (maybe "boolean" id t) [] [shows b]   
    xmlDefaultTag _ = "boolean"
    subElemDescriptor f tag = el tag f readableContent
    elemDescriptor f = el "boolean" f readableContent
    contentFinder f tag = mustHaveElem $ subElemDescriptor f tag
    
instance JavaRep String where
    representation _ = JRep Nothing (const "string") "string"
    representative = ""
    xmlSerialize t s = emit (maybe "string" id t) [] [showString s]
    xmlDefaultTag _ = "string"
    subElemDescriptor f tag = el tag f simpleContent
    elemDescriptor f = el "string" f simpleContent
    contentFinder f tag = mustHaveElem $ subElemDescriptor f tag
    
instance JavaRep a => JavaRep [a] where
    representation _ = JRep Nothing tpf v
        where tpf pkg = let JRep _ f _ = representation (representative :: a) in (f pkg) ++ "[]"
              (JRep _ _ v) = representation (representative :: a)
    representative = [ (representative :: a) ]
    xmlSerialize t [] = id
    xmlSerialize t l = emit (maybe (xmlDefaultTag (representative :: [a])) id t) [] (map (\ v -> xmlSerialize Nothing v) l)
    xmlDefaultTag _ = (xmlDefaultTag (representative :: a)) ++ "-array"
    subElemDescriptor f tag = el tag f (many $ elemDescriptor id)
    elemDescriptor f = el (xmlDefaultTag (representative :: [a])) f (many $ elemDescriptor id)
    contentFinder f tag = mustHaveElem $ subElemDescriptor f tag
    
instance JavaRep (Maybe Int) where
    representation _ = JRep Nothing (const "Integer") "Integer"
    representative = Just (representative :: Int)
    xmlSerialize t Nothing = id
    xmlSerialize t (Just v) = xmlSerialize t v
    xmlDefaultTag x = xmlDefaultTag (representative :: Int)
    subElemDescriptor f tag = subElemDescriptor (f . Just) tag
    elemDescriptor f = elemDescriptor (f . Just)
    contentFinder f tag = (canHaveElem $  (\ p e -> subElemDescriptor id tag p e)) >>= return . f
    
instance JavaRep (Maybe Float) where
    representation _ = JRep Nothing (const "Float") "Float"
    representative = Just (representative :: Float)
    xmlSerialize t Nothing = id
    xmlSerialize t (Just v) = xmlSerialize t v
    xmlDefaultTag x = xmlDefaultTag (representative :: Float)
    subElemDescriptor f tag = subElemDescriptor (f . Just) tag
    elemDescriptor f = elemDescriptor (f . Just)
    contentFinder f tag = (canHaveElem $  (\ p e -> subElemDescriptor id tag p e)) >>= return . f
    
instance JavaRep (Maybe Double) where
    representation _ = JRep Nothing (const "Double") "Double"
    representative = Just (representative :: Double)
    xmlSerialize t Nothing = id
    xmlSerialize t (Just v) = xmlSerialize t v
    xmlDefaultTag x = xmlDefaultTag (representative :: Double)
    subElemDescriptor f tag = subElemDescriptor (f . Just) tag
    elemDescriptor f = elemDescriptor (f . Just)
    contentFinder f tag = (canHaveElem $  (\ p e -> subElemDescriptor id tag p e)) >>= return . f

instance JavaRep (Maybe Char) where
    representation _ = JRep Nothing (const "Character") "Character"
    representative = Just (representative :: Char)
    xmlSerialize t Nothing = id
    xmlSerialize t (Just v) = xmlSerialize t v
    xmlDefaultTag x = xmlDefaultTag (representative :: Char)
    subElemDescriptor f tag = subElemDescriptor (f . Just) tag
    elemDescriptor f = elemDescriptor (f . Just)
    contentFinder f tag = (canHaveElem $  (\ p e -> subElemDescriptor id tag p e)) >>= return . f
    
instance JavaRep (Maybe Bool) where
    representation _ = JRep Nothing (const "Boolean") "Boolean"
    representative = Just (representative :: Bool)
    xmlSerialize t Nothing = id
    xmlSerialize t (Just v) = xmlSerialize t v
    xmlDefaultTag x = xmlDefaultTag (representative :: Bool)
    subElemDescriptor f tag = el tag (f . Just) boolContent
    elemDescriptor f = el (xmlDefaultTag "") (f . Just) boolContent
    contentFinder f tag = (canHaveElem $  (\ p e -> subElemDescriptor id tag p e)) >>= return . f
    
instance JavaRep a => JavaRep (Maybe a) where
    representation _ = representation (representative :: a)
    representative = Just (representative :: a)
    xmlSerialize t Nothing = id
    xmlSerialize t (Just v) = xmlSerialize t v
    xmlDefaultTag x = xmlDefaultTag (representative :: a)
    subElemDescriptor f tag = subElemDescriptor (f . Just) tag
    elemDescriptor f = elemDescriptor (f . Just)
    contentFinder f tag = (canHaveElem $  (\ p e -> subElemDescriptor id tag p e)) >>= return . f

javaRepCon = ConT ''JavaRep

mkRepresentation fun0 fun1 val2 = liftM head [d|representation _ = JRep $fun0 $fun1 $val2|]

deriveJavaRepTups  = mapM deriveJavaRepTup

deriveJavaRepTup n = do
        vs <- mapM newName (replicate n "v")
        let ctx =  mapM (appT (return javaRepCon) . varT) vs
        let typ = return $ AppT javaRepCon $ foldl AppT (TupleT n) (map VarT vs)
        let representativeD = valD (varP 'representative) (normalB $ return $ TupE (replicate n (VarE 'representative))) []
        let base = stringE' ("Tuple")
        let nameE = do
                es <- mapM  (((return jrepSimpleNameE) `appE`) . ((varE 'representation) `appE`) . (sigE (varE 'representative)) . varT) vs
                return $ concatE (base:es)
        let representationD = do
                pkgVarName <- newName "pkg"
                pkgE <- [e|$(varE pkgVarName)|]
                let pkgP = varP pkgVarName
                let pairs :: [(Int,Type)]
                    pairs = zipWith (\ i vnm -> (i,VarT vnm)) [1..n] vs
                let clsE = nameE >>= \ n -> return $ concatE 
                        ([stringE' "package ",pkgE, stringE' ";\npublic class ",n,stringE' "{\n"] ++
                        concatMap ((\ (i,t) -> [stringE' "    public ",
                                               jrepTypeGenE `AppE` (AppE (VarE 'representation) (SigE (VarE 'representative) t)) `AppE` pkgE,
                                               stringE' " ", stringE' ("el" ++ show i ++ ";\n")]) :: (Int,Type) -> [Exp])
                               pairs
                                ++
                         [stringE' "}\n"])
                let fun0 = [e|Just $(lam1E pkgP ([e|[($nameE,$clsE)]|]))|]
                let fun1 = lam1E pkgP (appE (appE concatV' (appE (appE concatV' (return pkgE)) (stringE "."))) nameE)
                mkRepresentation fun0 fun1 nameE
        let mkClause = do
                tagName <- newName "tag"
                vs <- mapM newName (replicate n "v")
                let tagE = (varE 'maybe) `appE` (nameE) `appE` (varE 'id) `appE` (varE tagName)
                let attrsE = (varE 'maybe) `appE` (listE []) 
                                           `appE` ((varE 'const) `appE` (listE [tupE [stringE "class",nameE]])) 
                                           `appE` (varE tagName)
                let lst = listE (zipWith (\ i x -> let nm = "el" ++ show i; v = varE x in [e|xmlSerialize (Just $(stringE nm)) $v|]) [1..n] vs)
                let expr = (varE 'emit) `appE` tagE `appE` attrsE `appE` lst
                clause [varP tagName, return (TupP (map VarP vs))] (normalB $ expr) []
        let xmlSerD = funD 'xmlSerialize [mkClause]
        let subElemDescriptorD = do
                tagNm <- newName "tag"
                fNm <- newName "f"
                vs <- mapM newName (replicate n "v")
                let tags = map (("el" ++) . show) [1..n]
                let terms = zip tags vs
                let stmts [] = return [noBindS ((varE 'return) `appE` (tupE (map varE vs)))]
                    stmts ((t,v):ts) = do
                        rest <- stmts ts
                        let stmt = bindS (varP v) ((varE 'contentFinder) `appE` (varE 'id) `appE` (stringE t))
                        return (stmt : rest)
                let fields = doE =<< (stmts terms)
                let elE = (varE 'elWith) `appE` (varE tagNm) `appE` ((varE 'const) `appE` (varE fNm))
                                         `appE` ((varE 'thisAttr) `appE` (stringE "class") `appE` nameE)
                                         `appE` ((varE 'comprises) `appE` fields)
                funD 'subElemDescriptor [clause [varP fNm,varP tagNm] (normalB elE) []]
        let elemDescriptorD = do
                fNm <- newName "f"
                vs <- mapM newName (replicate n "v")
                let tags = map (("el" ++) . show) [1..n]
                let terms = zip tags vs
                let stmts [] = return [noBindS ((varE 'return) `appE` (tupE (map varE vs)))]
                    stmts ((t,v):ts) = do
                        rest <- stmts ts
                        let stmt = bindS (varP v) ((varE 'contentFinder) `appE` (varE 'id) `appE` (stringE t))
                        return (stmt : rest)
                let fields = doE =<< (stmts terms)
                let elE = (varE 'el) `appE` nameE `appE` (varE fNm) `appE` ((varE 'comprises) `appE` fields)
                funD 'elemDescriptor [clause [varP fNm] (normalB elE) []]
        let contentFinderD = do
                fNm <- newName "f"
                tagNm <- newName "tag"
                funD 'contentFinder  
                    [clause [(varP fNm),(varP tagNm)] 
                                  (normalB ((varE 'mustHaveElem) `appE` ((varE 'subElemDescriptor) `appE` (varE fNm) `appE` (varE tagNm)))) []]
        instanceD ctx typ [representationD,representativeD,xmlSerD,funD 'xmlDefaultTag [clause [wildP] (normalB nameE) []], 
                           subElemDescriptorD, elemDescriptorD, contentFinderD]

deriveJavaRep nm = if nm == ''[] then return [] else
    do  info <- reify nm
        case info of
            TyConI d -> deriveInstance d >>= return . (:[])
            _ -> fail $ ("can't generate serializer for specified name: " ++ show nm)
    where
        deriveInstance (DataD _ tnm vs cs _) = checkAllFields cs >> instanceD ctx typ [representationD,representativeD,xmlSerializeD,dec1,
                                                                                       subElemDescriptorD,elemDescriptorD,contentFinderD]
            where ctx = mapM (appT (return javaRepCon) . varT) vs
                  typ = return $ AppT javaRepCon $ foldl AppT (ConT tnm) (map VarT vs)
                  mkRepresentative [] = [e|undefined|]
                  mkRepresentative (c:_) = getCInfo c >>= \ (cnm,fts) -> return $ foldl AppE (ConE cnm) (replicate (length fts) (VarE 'representative))
                  representativeD = firstDec [d|representative = $(mkRepresentative cs)|]
                  base = stringE' (nameBase tnm)
                  nameE = do
                      es <- mapM  (((return jrepSimpleNameE) `appE`) . ((varE 'representation) `appE`) . (sigE (varE 'representative)) . varT) vs
                      return $ concatE (base:es)
                  representationD = do
                     pkgVarName <- newName "pkg"
                     pkgE <- varE pkgVarName
                     let pkgP = varP pkgVarName
                     let clsE = nameE >>= \ n -> return $ concatE [stringE' "package ",pkgE,stringE' ";\npublic abstract class ",n,stringE' " {}\n"]
                     let mkSubClassExpr c = do 
                             (cnm,fts) <- getCInfo c
                             n <- nameE
                             let fes = concatMap mkFe fts
                                     where mkFe (nm,t) = [stringE' "    ",jrepTypeGenE `AppE` (AppE (VarE 'representation) (SigE (VarE 'representative) t)) `AppE` pkgE,
                                                          stringE' " ", stringE' $ nameBase nm, stringE' ";\n"]
                             let classRep = return $ concatE (
                                         [stringE' "package ",pkgE,stringE' ";\npublic class ",
                                         n,stringE' (nameBase cnm),stringE' " extends ",n,
                                         stringE' " {\n"] ++ fes ++ [stringE' "}\n"]
                                     )    
                             [e|($(nameE) ++ $(stringE $ nameBase cnm),$classRep)|]
                     let subClassExprs = map mkSubClassExpr cs
                     let fun0E = conE 'Just `appE` lam1E pkgP [e| ($nameE,$clsE) : $(listE subClassExprs) |] --(listE (clsE:subClassExprs))
                     let fun1E = lam1E pkgP (appE (appE concatV' (appE (appE concatV' (return pkgE)) (stringE "."))) nameE)
                     let val3 = nameE
                     mkRepresentation fun0E fun1E val3
                  xmlSerializeD = funD (mkName "xmlSerialize") (map mkClause cs)
                  dec1 = funD (mkName "xmlDefaultTag") [clause [wildP] (normalB nameE) []]
                  computeVarInfo (nm,_) = newName "x" >>= return . ((,) nm)
                  mkClause c = do
                      (cnm,fts) <- getCInfo c
                      vis <- mapM computeVarInfo fts
                      tagVarName <- newName "tag"
                      let cnmE = [| $(nameE) ++ $(stringE $ nameBase cnm)|]
                      let xmlSerializeE = varE 'xmlSerialize
                      let terms = map (\ (fnm,vnm) -> xmlSerializeE `appE` ((conE 'Just) `appE` (stringE $ nameBase fnm)) `appE` (varE vnm)) vis
                      name <- nameE
                      let maybeE = (varE 'maybe) `appE` cnmE `appE` (varE 'id) `appE` (varE tagVarName)
                      let attrs = (varE 'maybe) 
                                                `appE` ((listE []))
                                                `appE` ((varE 'const) `appE` (listE [tupE [stringE "class",cnmE]]))
                                                `appE` (varE tagVarName)
                      let exp = (varE 'emit) `appE` maybeE `appE` attrs
                                `appE` (listE (if null terms then [] else [foldl1 (\ x y -> (appE (appE (varE compose) x) y)) terms]))
                      clause [varP tagVarName,conP cnm (map (varP . snd) vis)] (normalB exp) []
                  subElemDescriptorD = do
                          fNm <- newName "f"
                          tagNm <- newName "tag"
                          funD 'subElemDescriptor 
                              [clause [(varP fNm),(varP tagNm)] (normalB (appE (varE 'choice) (choices tagNm fNm))) []]
                      where choices tagNm fNm = listE (map (mkChoice tagNm fNm) cs)
                            mkChoice tagNm fNm c = do
                                (cnm,fts) <- getCInfo c
                                let cnmE = [| $(nameE) ++ $(stringE $ nameBase cnm)|]
                                vis <- mapM computeVarInfo fts
                                (varE 'elWith) `appE` (varE tagNm) `appE` ((varE 'const) `appE` (varE fNm))
                                    `appE` ((varE 'thisAttr) `appE` (stringE "class") `appE` cnmE)
                                    `appE` ((varE 'comprises) `appE` (fields cnm vis))
                              where fields cnm vis = doE =<< (stmts cnm [] vis)
                                    stmts cnm vs [] = return [noBindS $ ((varE 'return) `appE` (foldl (appE) (conE cnm) (map varE $ reverse vs)))]
                                    stmts cnm vs ((fnm,_):vis) = do
                                        vn <- newName "v"
                                        rest <- stmts cnm (vn:vs) vis
                                        let stmt = bindS (varP vn) ((varE 'contentFinder) `appE` (varE 'id) `appE` (stringE $ nameBase fnm))
                                        return $ stmt : rest
                  elemDescriptorD = do
                          fNm <- newName "f"
                          funD 'elemDescriptor
                              [clause [(varP fNm)] (normalB (appE (varE 'choice) (choices fNm))) []]
                      where choices fNm = listE (map (mkChoice fNm) cs)
                            mkChoice fNm c = do
                                (cnm,fts) <- getCInfo c
                                let cnmE = [| $(nameE) ++ $(stringE $ nameBase cnm)|]
                                vis <- mapM computeVarInfo fts
                                (varE 'el) `appE` cnmE `appE` (varE fNm)
                                    `appE` ((varE 'comprises) `appE` (fields cnm vis))
                              where fields cnm vis = doE =<< (stmts cnm [] vis)
                                    stmts cnm vs [] = return [noBindS $ ((varE 'return) `appE` (foldl (appE) (conE cnm) (map varE $ reverse vs)))]
                                    stmts cnm vs ((fnm,_):vis) = do
                                        vn <- newName "v"
                                        rest <- stmts cnm (vn:vs) vis
                                        let stmt = bindS (varP vn) ((varE 'contentFinder) `appE` (varE 'id) `appE` (stringE $ nameBase fnm))
                                        return $ stmt : rest
                  contentFinderD = do
                          fNm <- newName "f"
                          tagNm <- newName "tag"
                          funD 'contentFinder 
                              [clause [(varP fNm),(varP tagNm)] 
                                  (normalB ((varE 'mustHaveElem) `appE` ((varE 'subElemDescriptor) `appE` (varE fNm) `appE` (varE tagNm)))) []]
        deriveInstance dec = fail ("can't derive instance for: " ++ show (ppr dec) ++ " (" ++ show dec ++ ")")
                      
getCInfo c =  case c of
    RecC cnm fvs -> return (cnm, map (\ (f,_,t) -> (f,t)) fvs)
    NormalC cnm vs -> return (cnm, zipWith (\ i (_,t) -> (mkName $ "el" ++ show i, t)) [1..(length vs)] vs)
    _ -> fail ("can't create java representation for " ++ show (ppr c))

checkField (_,t) | appliesVar t = fail ("can't generate serializer for type with type variables of kind other than '*'")
                 | otherwise       = return ()
checkFields = mapM checkField                

checkAllFields = mapM (\ c -> getCInfo c >>= \ (_,fts) -> checkFields fts)

appliesVar :: Type -> Bool
appliesVar = everything (||) (False `mkQ` chkApp)
    where chkApp :: Type -> Bool
          chkApp (AppT (VarT _) _) = True
          chkApp t = False 

cName :: Type -> [Name]
cName (ConT nm) = [nm]
cName _ = []

deriveRep nm = case nameBase nm of
--    '(':cs -> deriveJavaRepTups [(length cs)]
    _ -> deriveJavaRep nm
    
predef nm = nm `elem` [''Char,''Int,''Float,''Double,''String,''[],''Bool,''Maybe]

deriveAllJavaRepsFor :: String -> String -> Q Type -> Q [Dec]
deriveAllJavaRepsFor name pkg qtype = 
        go ([],[],[],id) qtype >>= \ (_,_,exprs,decs) -> return (decs [ValD (VarP $ mkName name) (NormalB (ListE exprs)) []])
    where go acc@(seenNm,seenT,exprs,decs) qtype = do
                   typ <- qtype
                   let texprs = faTypeToTcAndArgs typ
                   foldM go1 acc texprs
               where go1 acc@(seenNm,seenT,exprs,decs) (nm,args) | (nm,args) `elem` seenT || predef nm = return acc
                                                                 | otherwise = do
                         info <- reify nm
                         case info of
                             TyConI d -> do 
                                 let qtype = return (foldl AppT (ConT nm) args)
                                 let representativeExpr = sigE [e|representative|] qtype
                                 expr1 <- [e| let Just f0 = jrepClassGen (representation $representativeExpr) 
                                              in f0 $(stringE pkg)|]
                                 (ts,decs1) <- handleDec d
                                 foldM go (nm:seenNm,(nm,args):seenT,expr1:exprs,decs . (decs1++)) (map return ts)
                             PrimTyConI _ _ _ -> return acc
                             other    -> fail $ ("can't do derivation for specified name: " ++ show nm ++ " = " ++ (show . ppr) other
                                                ++ " - " ++ show other)
                         where
                             mismatchFailure nm =  fail ("can't do derivation on partially applied type constructor " ++ show nm)
                             handleDec dec@(DataD _ nm ps cs _) | length ps /= length args = mismatchFailure nm
                                                                | otherwise = do
                                                                     decs1 <- if nm `elem` seenNm then return [] else deriveRep nm
                                                                     return $ (findApplicationsInDec args dec,decs1)
                             handleDec dec@(NewtypeD _ nm ps c _) | length ps /= length args = mismatchFailure nm
                                                                  | otherwise = do
                                                                     decs1 <- if nm `elem` seenNm then return [] else deriveRep nm
                                                                     return $ (findApplicationsInDec args dec,decs1)
                             handleDec dec@(TySynD _ ps t) | length ps /= length args = mismatchFailure nm
                                                           | otherwise = return (findApps (zip ps args) [t],[])
                                                           
faTypeToTcAndArgs t = go [] t
    where go args (ConT nm) = [(nm,args)]
          go args (AppT t0 t1) = go (t1:args) t0 ++ go [] t1
          
findApplicationsInDec args (NewtypeD _ nm ps c _) = findApplicationsInConstructor (zip ps args) c
findApplicationsInDec args (DataD _ nm ps cs _) = nub $ concatMap (findApplicationsInConstructor (zip ps args)) cs

findApplicationsInConstructor :: [(Name,Type)] -> Con -> [Type]
findApplicationsInConstructor args c = case c of
        (NormalC nm fs) -> findApps args [ t | (_,t) <- fs ]
        (RecC nm fs) -> findApps args [ t | (_,_,t) <- fs ]
        (InfixC (_,t0) nm (_,t1)) -> findApps args [t0,t1]
        _ -> []

findApps args = nub . (map (subst args)) . findApplicationsInList

subst args t@(VarT nm) = maybe t id (lookup nm args)
subst args (AppT t0 t1) = AppT (subst args t0) (subst args t1)
subst args t = t

findApplicationsInList ts = foldl findFullyAppliedTypes [] ts

findFullyAppliedTypes ts t@(VarT _) = t:ts
findFullyAppliedTypes ts t = findApplications ts t

findApplications ts t@(ConT nm) | t `elem` ts && nm `notElem` [''[]] = ts
                                | otherwise   = t:ts
findApplications ts t@(AppT  _ t1) | t `elem` ts = ts
                                   | otherwise   = t : findApplications ts t1 
findApplications ts _ = ts
                                   
firstDec :: Q [Dec] -> Q Dec
firstDec = liftM head
