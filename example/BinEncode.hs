{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, EmptyDataDecls, TemplateHaskell #-}
module Main where
import Generics.Instant
import Data.List
import Generics.Instant.Derive

derive ''Either
deriveWith ''()   [Just "Unit_Unit"]
deriveWith ''(,,) [Just "Triple_Triple"]

class Bin a where
    toBin :: a -> [Int]
    fromBin :: [Int] -> (a, [Int])

instance Bin U where
    toBin   U  = []
    fromBin xs = (U, [])

instance (Bin a, Bin b) => Bin (a :+: b) where
    toBin (L a) = 0:toBin a
    toBin (R b) = 1:toBin b
    fromBin (0:bin) = let (a, bin') = fromBin bin in (L a, bin')
    fromBin (1:bin) = let (b, bin') = fromBin bin in (R b, bin')

instance (Bin a, Bin b) => Bin (a :*: b) where
    toBin (a :*: b) = toBin a ++ toBin b
    fromBin bin     =
      let (a, bin')  = fromBin bin
          (b, bin'') = fromBin bin'
      in (a :*: b, bin'')

instance Bin a => Bin (Rec a) where
    toBin (Rec r) = toBin r
    fromBin bin   = let (r, bin') = fromBin bin in (Rec r, bin')

instance Bin a => Bin (C c a) where
    toBin (C a)  = toBin a
    fromBin bin  = let (a, bin') = fromBin bin in (C a, bin')

instance Bin a => Bin (Var a) where
    toBin (Var a) = toBin a
    fromBin bin   = let (x, bin') = fromBin bin in (Var x, bin')

def_toBin :: (Representable a, Bin (Rep a)) => a -> [Int]
def_toBin = toBin . from

def_fromBin :: (Representable a, Bin (Rep a)) => [Int] -> (a, [Int])
def_fromBin bin = let (rep, bin') = fromBin bin in (to rep, bin')


instance Bin Int where
    toBin i   = let sign   = if i < 0 then 1 else 0
                    i'     = abs i
                    binary = toBits i'
                in take (bitCount + 1) (sign:binary ++ replicate bitCount 0)
    fromBin a = let (s:as, bs) = splitAt (1+bitCount) a
                    sign = [1, -1] !! s
                in (sign*fromBits as, bs)

instance Bin Char where
  toBin      = take 21 . (++replicate 21 0) . toBits . fromEnum
  fromBin bs = let (ch, bs') = splitAt 21 bs in (toEnum $ fromBits ch, bs')

instance Bin Bool where
  toBin True  = [1]
  toBin False = [0]
  fromBin (1:xs) = (True, xs)
  fromBin (0:xs) = (False, xs)

instance Bin () where { toBin = def_toBin; fromBin = def_fromBin }
instance Bin a => Bin [a] where { toBin = def_toBin; fromBin = def_fromBin }
instance Bin a => Bin (Maybe a) where { toBin = def_toBin; fromBin = def_fromBin }
instance (Bin a, Bin b) => Bin (Either a b) where { toBin = def_toBin; fromBin = def_fromBin }
instance (Bin a, Bin b) => Bin (a, b) where { toBin = def_toBin; fromBin = def_fromBin }
instance (Bin a, Bin b, Bin c) => Bin (a, b, c) where { toBin = def_toBin; fromBin = def_fromBin }

unfoldrStep :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> Maybe (a, b)
unfoldrStep p f g x | p x       = Nothing
                    | otherwise = Just (f x, g x)

newtype Wrap a = Wrap {unWrap :: a} deriving (Show, Eq, Ord)

bitCount = ceiling $ logBase 2 $ fromIntegral (maxBound::Int)
toBits   = unfoldr (unfoldrStep (==0) (`mod`2) (`div`2))
fromBits = foldr (\a b -> a+2*b) 0

