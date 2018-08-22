module RuckSack
    ( RuckSack
    , add
    , isEmpty
    , mkRuckSack
    , size
    ) where

data RuckSack a = RuckSack Int [a] deriving (Eq, Show)

instance Functor RuckSack where
    fmap f (RuckSack count xs) = RuckSack count $ fmap f xs

instance Monoid (RuckSack a) where
    (RuckSack aLength a) `mappend` (RuckSack bLength b) =
        RuckSack (aLength + bLength) (a `mappend` b)
    mempty = mkRuckSack

mkRuckSack :: RuckSack a
mkRuckSack = RuckSack 0 []

add :: a ->  RuckSack a -> RuckSack a
add x (RuckSack count xs) = RuckSack (count + 1) (x:xs)

isEmpty :: RuckSack a -> Bool
isEmpty (RuckSack 0 _) = True
isEmpty _ = False

size :: RuckSack a -> Int
size (RuckSack count _) = count
