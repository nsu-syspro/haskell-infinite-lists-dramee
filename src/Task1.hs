{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)
-- Import unfoldr for generating lists
import Data.List (unfoldr)

-- * Type definitions

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

-- | Forest (i.e. list of 'Tree's)
type Forest a = [Tree a]

-- | Tree traversal order
data Order = PreOrder | InOrder | PostOrder
  deriving Show

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

intercalate :: [a] -> [[a]] -> [a]
intercalate _   []     = []
intercalate sep (x:xs) = x ++ concatMap (sep ++) xs

-- * Function definitions

-- | Returns values of given 'Tree' in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> torder PreOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "A.B.."
-- >>> torder InOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".A.B."
-- >>> torder PostOrder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".BA"
--
torder :: Order    -- ^ Order of resulting traversal
       -> Maybe a  -- ^ Optional leaf value
       -> Tree a   -- ^ Tree to traverse
       -> [a]      -- ^ List of values in specified order

torder _     m Leaf                = maybeToList m
torder PreOrder m (Branch a left right) = 
    a : (torder PreOrder m left ++ torder PreOrder m right)
torder InOrder  m (Branch a left right) = 
    torder InOrder m left ++ [a] ++ torder InOrder m right
torder PostOrder m (Branch a left right) = 
    torder PostOrder m left ++ torder PostOrder m right ++ [a]

-- | Returns values of given 'Forest' separated by optional separator
-- where each 'Tree' is traversed in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|C..|A.B.."
-- >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|.C.|.A.B."
-- >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|..C|...BA"
--
forder :: Order     -- ^ Order of tree traversal
       -> Maybe a   -- ^ Optional separator between resulting tree orders
       -> Maybe a   -- ^ Optional leaf value
       -> Forest a  -- ^ List of trees to traverse
       -> [a]       -- ^ List of values in specified tree order
forder order msep mleaf forest = intercalate (maybeToList msep) (map (torder order mleaf) forest)

-- * Task 1: Infinite sequences using unfoldr

-- | Infinite list of natural numbers (excluding zero)
-- First 10 numbers: take 10 nats ==> [1,2,3,4,5,6,7,8,9,10]
nats :: [Integer]
nats = unfoldr (\x -> Just (x, x + 1)) 1

-- | Infinite Fibonacci sequence starting with 0
-- First 10 numbers: take 10 fibs ==> [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)

-- | Infinite list of prime numbers using Sieve of Eratosthenes
-- First 10 numbers: take 10 primes ==> [2,3,5,7,11,13,17,19,23,29]
primes :: [Integer]
primes = unfoldr sieve [2..]
  where
    sieve :: [Integer] -> Maybe (Integer, [Integer])
    sieve (p:xs) = Just (p, filter (\x -> x `mod` p /= 0) xs)
    sieve []     = Nothing  -- never happens for infinite list