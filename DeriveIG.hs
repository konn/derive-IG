{-# LANGUAGE TemplateHaskell, PatternGuards, ScopedTypeVariables, FlexibleContexts #-}
module DeriveIG where
import Reify
import Generics.Instant
import Language.Haskell.TH
import Control.Monad
import Data.Char
import qualified Data.Generics as SYB

normalizeDec :: Dec -> Dec
normalizeDec dec@(DataD _ _ _ _ _)         = dec
normalizeDec (NewtypeD cxt nm vars con xs) = DataD cxt nm vars [con] xs

normalizeCon :: Con -> Con
normalizeCon con@(NormalC _ _)     = con
normalizeCon (RecC name vsts)      = NormalC name $ map (\(n,s,t) -> (s,t)) vsts
normalizeCon (InfixC st1 name st2) = NormalC name [st1, st2]

deriveIG :: Name -> Q [Dec]
deriveIG name = do
  TyConI typ <- reify name
  let DataD cxt dName vars cons _ = normalizeDec typ
  inst <- appsT (conT dName) (map (return . bndlToType) vars)
  let cons' = map normalizeCon cons
      decs  = concatMap (buildRep dName inst . normalizeCon) cons'
      dats  = [d | d@(DataD _ _ _ _ _) <- decs]
      cnsts = [c | c@(InstanceD _ _ _) <- decs]
      rep   = [r | (TySynInstD _ _ r) <- decs] 
      tySyn = tySynInstD ''Rep [return inst] $ return $ foldr1 (op ''(:+:)) rep
  exps <- buildQ (foldr1 (op ''(:+:)) rep) :: Q [Exp]
  pats <- buildQ (foldr1 (op ''(:+:)) rep) :: Q [Pat]
  let from = FunD (mkName "from") $ zipWith mkFrom cons' exps
      to   = FunD (mkName "to") $ zipWith mkTo pats cons'
  dec <- instanceD (return cxt) (appT (conT ''Representable) (return inst)) [tySyn, return from, return to]
  return $ dec:dats++cnsts

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

buildRep :: Name -> Type -> Con -> [Dec]
buildRep typName inst (NormalC name sts) = [conDataDec, conInstDec, typInsDec]
  where conDataName = mkName $ replace '.' '_' (show typName) ++ "_" ++ nameBase name
        conDataDec  = DataD [] conDataName [] [] []
        cnstrType   = AppT (ConT ''C) (ConT conDataName)
        conInstDec  = InstanceD [] (AppT (ConT ''Constructor) (ConT conDataName)) 
                        [FunD 'conName [Clause [WildP] (NormalB (LitE $ StringL $ show name)) []] ]
        repType | null sts  = AppT cnstrType (ConT ''U)
                | otherwise = AppT cnstrType . foldr1 (op ''(:*:)) . map (varOrRec . snd) $ sts
        typInsDec  = TySynInstD ''Rep undefined repType
        varOrRec n | n == inst = AppT (ConT ''Rec) n
                   | otherwise = AppT (ConT ''Var) n

class Tree a where
    con :: Name -> [a] -> a
    var :: Name -> a

instance Tree Exp where
    con name xs = foldl1 AppE (ConE name:xs)
    var         = VarE

instance Tree Pat where
    con = ConP
    var = VarP

buildQ :: Tree a => Type -> Q [a]
buildQ (AppT (AppT (ConT c) _) ty) | c == ''C = do {bs <- buildQ ty; return [con 'C bs]}
buildQ (AppT (AppT (ConT c) a) b)
    | c == ''(:+:) = do
       l  <- buildQ a
       rs <- liftM (map (con 'R . return)) $ (buildQ b)
       return (con 'L l:rs)
    | c == ''(:*:) = do
       ls <- buildQ a
       rs <- buildQ b
       return [con '(:*:) (ls ++ rs)]
buildQ (AppT (ConT c) a) | c == ''Var = liftM ((:[]) . con 'Var . take 1) $ buildQ a
                         | c == ''Rec = liftM ((:[]) . con 'Rec . take 1) $ buildQ a
buildQ v = do
  name <- newName "_param"
  return [var name]

type2Name (ConT nm) = nm
type2Name (VarT nm) = nm

mkFrom :: Con -> Exp -> Clause
mkFrom (NormalC cName _) exp = let ps = SYB.listify ((=="_param") . nameBase) exp
                               in Clause [ConP cName (map VarP ps)] (NormalB exp) []

mkTo :: Pat -> Con -> Clause
mkTo pat (NormalC cName _) = let ps = SYB.listify ((=="_param") . nameBase) pat
                             in Clause [pat] (NormalB $ foldl AppE (ConE cName) $ map VarE ps) []

