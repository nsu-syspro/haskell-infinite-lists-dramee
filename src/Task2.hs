{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))
import Data.Foldable (Foldable(..), toList)

-- * Type definitions

-- | Ordering enumeration
-- Overrides Prelude's Ordering
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
compare :: Ord a => Cmp a
compare x y
  | x < y     = LT
  | x > y     = GT
  | otherwise = EQ

-- | Conversion of list to binary search tree
listToBST :: Cmp a -> [a] -> Tree a
listToBST _ []     = Leaf
listToBST cmp (x:xs) = tinsert cmp x (listToBST cmp xs)

-- | Conversion from binary search tree to list
bstToList :: Tree a -> [a]
bstToList Leaf = []
bstToList (Branch a left right) = bstToList left ++ [a] ++ bstToList right

-- | Tests whether given tree is a valid binary search tree
isSorted :: Cmp a -> [a] -> Bool
isSorted _ [] = True
isSorted _ [_] = True
isSorted cmp (x:y:xs) = (cmp x y == LT) && isSorted cmp (y:xs)

isBST :: Cmp a -> Tree a -> Bool
isBST cmp tree = isSorted cmp (bstToList tree)

-- | Searches given binary search tree for given value
-- Returns found value wrapped in 'Just' or 'Nothing'
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup cmp x (Branch a left right) = case cmp x a of
    EQ -> Just a
    LT -> tlookup cmp x left
    GT -> tlookup cmp x right

-- | Inserts given value into binary search tree
-- preserving its BST properties
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ x Leaf = Branch x Leaf Leaf
tinsert cmp x (Branch a left right) = case cmp x a of
    LT -> Branch a (tinsert cmp x left) right
    GT -> Branch a left (tinsert cmp x right)
    EQ -> Branch x left right

-- | Deletes given value from binary search tree
-- preserving its BST properties
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete cmp x (Branch y left right) = case cmp x y of
    LT -> Branch y (tdelete cmp x left) right
    GT -> Branch y left (tdelete cmp x right)
    EQ -> case (left, right) of
        (Leaf, _) -> right
        (_, Leaf) -> left
        _         -> let m = findMin right
                     in Branch m left (tdelete cmp m right)
  where
    findMin :: Tree a -> a
    findMin (Branch z Leaf _) = z
    findMin (Branch _ left' _) = findMin left'
    findMin Leaf = error "findMin: пустое дерево"

-- * Task 2: Infinite streams

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

-- | Functor instance for Stream
instance Functor Stream where
  fmap f (Stream x xs) = Stream (f x) (fmap f xs)

-- | Foldable instance for Stream (enables toList)
instance Foldable Stream where
  foldr f z (Stream x xs) = f x (foldr f z xs)

-- | Show instance: display first 10 elements
instance Show a => Show (Stream a) where
  show s = show (take 10 (toList s))

-- | Convert finite list to Stream, repeating the default after end
fromList :: a -> [a] -> Stream a
fromList def xs = case xs of
  (y:ys) -> Stream y (fromList def ys)
  []     -> let s = Stream def s in s

-- | Build Stream by unfolding seed
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f b = let (a, b') = f b
             in Stream a (unfold f b')

-- | Filter for Stream
defilter :: (a -> Bool) -> Stream a -> Stream a
defilter p (Stream x xs)
  | p x       = Stream x (defilter p xs)
  | otherwise = defilter p xs

-- | Infinite natural numbers (starting from 1)
nats :: Stream Integer
nats = unfold (\x -> (x, x + 1)) 1

-- | Infinite Fibonacci sequence (0,1,1,2,...)
fibs :: Stream Integer
fibs = unfold (\(a, b) -> (a, (b, a + b))) (0, 1)

-- | Infinite primes using Sieve of Eratosthenes
primes :: Stream Integer
primes = sieve (unfold (\x -> (x, x + 1)) 2)
  where
    sieve (Stream p ps) = Stream p (sieve (defilter (\x -> x `mod` p /= 0) ps))
