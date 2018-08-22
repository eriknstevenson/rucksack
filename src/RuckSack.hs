module RuckSack
    ( RuckSack
    , add
    , isEmpty
    , mkRuckSack
    , size
    ) where

newtype RuckSack a = RuckSack [a] deriving (Eq, Show)

instance Functor RuckSack where
    fmap f (RuckSack xs) = RuckSack $ fmap f xs

instance Monoid (RuckSack a) where
    (RuckSack a) `mappend` (RuckSack b) = RuckSack $ a `mappend` b
    mempty = mkRuckSack

mkRuckSack :: RuckSack a
mkRuckSack = RuckSack []

add :: a ->  RuckSack a -> RuckSack a
add x (RuckSack xs) = RuckSack (x:xs)

isEmpty :: RuckSack a -> Bool
isEmpty (RuckSack []) = True
isEmpty _ = False

size :: RuckSack a -> Int
size (RuckSack xs) = length xs
