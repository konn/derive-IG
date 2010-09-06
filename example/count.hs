{-# LANGUAGE TemplateHaskell, EmptyDataDecls, FlexibleContexts, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances ,TypeSynonymInstances, TypeFamilies, TypeOperators #-}
import Generics.Instant
import Language.Haskell.TH
import Generics.Instant.Derive

data Expr = N Int
          | V String
          | P Expr Expr
            deriving (Show, Eq, Ord)

derive ''Expr
deriveWith ''(,,) [Just "Triple_Con"]
deriveWith ''()   [Just "Unit_Unit"]


class Count a where
    count :: a -> Int
    count _ = 1

instance Count Int
instance Count Char
instance Count Bool
instance Count String

instance Count U
instance Count (C con U)
instance Count a => Count (Var a) where
    count (Var a) = count a
instance (Count a, Count b) => Count (a :*: b) where
    count (a :*: b) = count a + count b
instance (Count a) => Count (Rec a) where
    count (Rec a) = count a
instance Count a => Count (C con a) where
    count (C a) = 1 + count a
instance (Count a, Count b) => Count (a :+: b) where
    count (L a) = count a
    count (R b) = count b

dft_count :: (Representable a, Count (Rep a)) => a -> Int
dft_count = count . from

instance Count () where count = dft_count
instance Count a => Count (Maybe a) where count = dft_count
instance Count a => Count [a] where count = dft_count
instance (Count a, Count b) => Count (a, b) where count = dft_count
instance (Count a, Count b, Count c) => Count (a, b, c) where count = dft_count
instance Count Expr where count = dft_count

