{-# LANGUAGE TypeOperators, EmptyDataDecls, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
import Generics.Instant
import DeriveIG

-- 直に木を舐めるだけの実装
-- everywhere の用に任意の構造体中の木を舐めたければ
-- 各所のコメントを参考にに実装してください
data Expr = Num Int
          | Val String
          | Plus Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div   Expr Expr
            deriving (Show, Eq)

deriveIG ''Expr

class Normalize a where
    normalize :: a -> a
    normalize = id

dft_normalize :: (Representable a, Normalize (Rep a)) => a -> a
dft_normalize = to . normalize . from

instance Normalize U

-- 任意の構造中の木を舐める様にするには，Var の中も再帰的に舐める必要がある．
-- 今回は構文木木だけを考えているので，Varの中身は舐めない．
instance Normalize (Var a)

instance (Normalize a, Normalize b) => Normalize (a :+: b) where
    normalize (L a) = L (normalize a)
    normalize (R b) = R (normalize b)

instance (Normalize a, Normalize b) => Normalize (a :*: b) where
    normalize (a :*: b) = normalize a :*: normalize b

instance Normalize a => Normalize (Rec a) where
    normalize (Rec a) = Rec (normalize a)

instance Normalize a => Normalize (C con a) where
    normalize (C a) = C (normalize a)

{- Varの中も舐めるのなら，次の宣言が必要
-- normalize のデフォルト定義が id なので，instance と書くだけでいい
-- instance Normalize Int
-- instance Normalize Char
-- 他のデータ中の木を舐めたければ，次の宣言も追加
-- instance Normalzie a => Normalize (Maybe a) where normalize = dft_normalize
-- instance Normalzie a => Normalize [a] where normalize = dft_normalize
-- etc, etc...
-}

instance Normalize Expr where
  normalize x = case dft_normalize x of
                  Plus (Num n) (Num m)  -> Num (n + m)
                  Multi (Num n) (Num m) -> Num (n * m)
                  Minus (Num n) (Num m) -> Num(n - m)
                  Div (Num n) (Num m)   -> Num(n `div` m)
                  x                     -> x

-- 2 * (3 + 2 * 5) = 26
tree1 = Multi (Num 2) (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- a * (3 + 2 * 5) = a * 13
tree2 = Multi (Val "a") (Plus (Num 3) (Multi (Num 2) (Num 5)))

tree3 = Multi (Multi (Num 2) (Plus (Num 5) (Num 2))) tree2