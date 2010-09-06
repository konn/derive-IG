{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, ViewPatterns, FlexibleContexts #-}
module Generics.Instant.Derive 
    (derive, derives, deriveWith, deriveCon, deriveConWith, deriveRep, deriveRepWith) where
import Generics.Instant
import Language.Haskell.TH 
import Language.Haskell.TH.Syntax (lift)
import Control.Monad
import Data.Char (isLower)

normalizeDec :: Dec -> Dec
normalizeDec dec@(DataD _ _ _ _ _)         = dec
normalizeDec (NewtypeD cxt nm vars con xs) = DataD cxt nm vars [con] xs

normalizeCon :: Con -> Con
normalizeCon con@(NormalC _ _)     = con
normalizeCon (RecC name vsts)      = NormalC name $ map (\(n,s,t) -> (s,t)) vsts
normalizeCon (InfixC st1 name st2) = NormalC name [st1, st2]

-- |Generate Constructor and Representable instance for 
derive :: Name -> Q [Dec]
derive typName = deriveWith typName []

-- | 
derives :: [Name] -> Q [Dec]
derives = liftM concat . mapM derive

deriveWith :: Name -> [Maybe String] -> Q [Dec]
deriveWith typName conNames = do
  xs <- deriveConWith' typName conNames
  let consts = concatMap fst xs
      cNames = map (Just . show . snd) xs
  reps <- deriveRepWith typName cNames
  return $ consts ++ reps

consWithNames :: Dec -> [Maybe String] -> [(Con, Name)]
consWithNames (normalizeDec -> DataD _ dName _ cons _) conNames = map step (zip cons conNames)
  where
    step (cons, name) = 
        let c@(NormalC cName _) = normalizeCon cons 
            altName    = mkName $ replace '.' '_' (show name) ++ "_" ++ nameBase cName
        in (c, maybe altName mkName name)

deriveRep :: Name -> Q [Dec]
deriveRep name = deriveRepWith name (repeat Nothing)

deriveRepWith :: Name -> [Maybe String] -> Q [Dec]
deriveRepWith name conNames = do
  TyConI info <- reify name
  let d@(DataD cxt dName vars cons xs) = normalizeDec info
  inst <- appsT (conT dName) (map (return . bndlToType) vars)
  reps <- forM (consWithNames d conNames) $ \(con, nm) -> do
    let NormalC cName sts   = normalizeCon con
        varOrRec n | n == inst = AppT (ConT ''Rec) n
                   | otherwise = AppT (ConT ''Var) n
        repType | null sts  = return $ ConT ''U
                | otherwise = return $ foldr1 (op ''(:*:)) . map (varOrRec . snd) $ sts
    appT (appT (conT ''C) (conT nm)) repType
  tySyn <- tySynInstD ''Rep [return inst] $ return $ foldr1 (op ''(:+:)) reps
  exps <- buildQ (foldr1 (op ''(:+:)) reps) :: Q [([Name], Exp)]
  pats <- buildQ (foldr1 (op ''(:+:)) reps) :: Q [([Name], Pat)]
  let from = funD (mkName "from") $ zipWith mkFrom cons exps
      to   = funD (mkName "to") $ zipWith mkTo pats cons
  dec <- instanceD (return cxt) (appT (conT ''Representable) (return inst)) [return tySyn, from, to]
  return [dec]
  

deriveCon :: Name -> Q [Dec]
deriveCon name = deriveConWith name (repeat Nothing)

deriveConWith :: Name -> [Maybe String] -> Q [Dec]
deriveConWith name' conNames = liftM (concatMap fst) $ deriveConWith' name' conNames

deriveConWith' :: Name -> [Maybe String] -> Q [([Dec], Name)]
deriveConWith' name' conNames = do
  TyConI info <- reify name'
  let DataD cxt name vars cons' xs = normalizeDec info
  forM (zip cons' $ conNames ++ repeat Nothing) $ \(conData, aliasName) ->do
    let (cName, args, fixity, isRec) =
          case conData of
            NormalC cName args   -> (cName, args, conE 'Prefix, False)
            RecC cName vsts      -> (cName, map (\(n,s,t) -> (s,t)) vsts, conE 'Prefix, True)
            InfixC st1 cName st2 -> (cName, [st1, st2], appsE [conE 'Infix, conE 'NotAssociative, litE $ integerL 0], False)
        altName    = mkName $ replace '.' '_' (show name) ++ "_" ++ nameBase cName
        conDataName  = maybe altName mkName aliasName
    conDataDec <- dataD (return []) conDataName [] [] []
    cnstrType  <- appT (conT ''C) (conT conDataName)
    conInstDec <- instanceD (return []) (appT (conT ''Constructor) (conT conDataName)) 
                        [ funD 'conName [clause [wildP] (normalB (litE $ stringL $ show cName)) []]
                        , funD 'conFixity [clause [wildP] (normalB $  fixity) []]
                        , funD 'conIsRecord [clause [wildP] (normalB $ lift isRec) []]
                        ]
    return ([conDataDec, conInstDec], conDataName)


bndlToType :: TyVarBndr -> Type
bndlToType (PlainTV nm)    = nameToType nm
bndlToType (KindedTV nm _) = nameToType nm
nameToType name | isLower $ head (nameBase name) = VarT name
                | otherwise                      = ConT name

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT

replace :: Eq a => a -> a -> [a] -> [a]
replace from to = map step
  where
    step c | c == from = to
           | otherwise = c

op :: Name -> Type -> Type -> Type
op name = AppT . AppT (ConT name)

class Tree a where
    con :: Name -> [a] -> a
    var :: Name -> a

instance Tree Exp where
    con name xs = foldl1 AppE (ConE name:xs)
    var         = VarE

instance Tree Pat where
    con = ConP
    var = VarP

buildQ :: Tree a => Type -> Q [([Name], a)]
buildQ (AppT (AppT (ConT c) _) ty) | c == ''C = do {(nms,bs):_ <- buildQ ty; return [(nms, con 'C [bs])]}
buildQ (AppT (AppT (ConT c) a) b)
    | c == ''(:+:) = do
       (lname,l):_  <- buildQ a
       rs <- buildQ b
       return ((lname, con 'L [l]):map (\(n, exp) -> (n, con 'R [exp])) rs)
    | c == ''(:*:) = do
       ls <- buildQ a
       rs <- buildQ b
       return [(concatMap fst ls ++ concatMap fst rs, con '(:*:) (map snd ls ++ map snd rs))]
buildQ (AppT (ConT c) a)
    | c == ''Var = do
        ((nms, l):_) <- buildQ a
        return [(nms, con 'Var [l])] 
    | c == ''Rec = do
        ((nms, r):_) <-buildQ a
        return [(nms, con 'Rec [r])]
buildQ (ConT name) | name == ''U = return [([], con 'U [])]
buildQ v = do
  name <- newName "_param"
  return [([name], var name)]

mkFrom :: Con -> ([Name], Exp) -> ClauseQ
mkFrom (normalizeCon -> NormalC cName _) (names, exp) =
    clause [conP cName (map varP names)] (normalB $ return exp) []

mkTo :: ([Name], Pat) -> Con -> ClauseQ
mkTo (names, pat) (normalizeCon -> NormalC cName _) = 
    clause [return pat] (normalB $ appsE (conE cName:map varE names)) []

