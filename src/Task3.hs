{-# OPTIONS_GHC -Wall #-}
module Task3 where

import Prelude hiding (compare, foldl, foldr, Ordering(..))
import Data.Ratio ((%))
import Task1 (Tree(..))
import Task2 (Stream(..), fromList, unfold, defilter, compare, Ordering)

-- | Tree-based map
type Map k v = Tree (k, v)

-- | Helper comparison function that compares only the keys.
compareKeys :: Ord k => (k, v) -> (k, v) -> Ordering
compareKeys (k1, _) (k2, _) = compare k1 k2

-- | Constructs a map from an association list.
listToMap :: Ord k => [(k, v)] -> Map k v
listToMap = listToBST compareKeys
  where listToBST = Task2.listToBST

-- | Converts a map into a sorted association list.
mapToList :: Map k v -> [(k, v)]
mapToList = bstToList
  where bstToList = Task2.bstToList

-- | Looks up a value by key in the map.
mlookup :: Ord k => k -> Map k v -> Maybe v
mlookup key m = case tlookup compareKeys (key, undefined) m of
                  Just (_, v) -> Just v
                  Nothing     -> Nothing
  where tlookup = Task2.tlookup

-- | Inserts a key and value into the map.
minsert :: Ord k => k -> v -> Map k v -> Map k v
minsert key val = tinsert compareKeys (key, val)
  where tinsert = Task2.tinsert

-- | Deletes a key (and its associated value) from the map.
mdelete :: Ord k => k -> Map k v -> Map k v
mdelete key = tdelete compareKeys (key, undefined)
  where tdelete = Task2.tdelete

-- * Generating functions

-- | Power series represented as infinite stream of coefficients
--   a0 + a1 x + a2 x^2 + ...
newtype Series a = Series { coefficients :: Stream a }

-- | The formal variable x = 0 + 1*x + 0*x^2 + ...
x :: Num a => Series a
x = Series $ Stream 0 (Stream 1 (defilter (const True) (Stream 0 undefined)))
  -- better: Stream 0 (Stream 1 (repeat 0)), but we use fromList

-- Helper to produce infinite zeros
zeros :: Num a => Stream a
zeros = unfold (\_ -> (0, ())) ()

-- define x properly
x = Series (fromList 0 [0,1])
  -- coefficients [0,1,0,0,...]

-- | Utility: multiply series by constant
infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
n *: Series s = Series (fmap (n*) s)

instance Num a => Num (Series a) where
  fromInteger n = Series (fromList 0 [fromInteger n])
  negate (Series s) = Series (fmap negate s)
  Series a + Series b = Series (zipStreams (+) a b)
  Series (Stream a as) * Series b@(Stream b0 bs) =
    Series (Stream (a * b0) (zipStreams (+) (fmap (a*) bs) (coefficients (Series as * Series b))))
    where Series as = Series as
  abs = id  -- not used
  signum = const (Series (fromList 0 [1]))

-- | Zip two streams with a function
zipStreams :: (a -> a -> a) -> Stream a -> Stream a -> Stream a
zipStreams f (Stream x xs) (Stream y ys) = Stream (f x y) (zipStreams f xs ys)

instance Fractional a => Fractional (Series a) where
  fromRational r = Series (fromList 0 [fromRational r])
  Series a / Series b@(Stream b0 bs) =
    Series (q0 `Stream` qs)
    where q0 = a0 / b0
          Stream a0 as = a
          qs = coefficients $ Series as - Series (fromList 0 [q0]) * Series bs / Series b

-- | Generate integer stream from Series of Ratio Integer
gen :: Series (Ratio Integer) -> Stream Integer
gen (Series s) = fmap numerator s
  where numerator r = let n = (Data.Ratio.numerator r) in n

-- | Infinite stream of ones
ones :: Stream Integer
ones = gen (1 / (1 - Series x))

-- | Natural numbers: generating function 1/(1-x)^2
nats :: Stream Integer
nats = gen (1 / ((1 - Series x) * (1 - Series x)))

-- | Fibonacci numbers: generating function x/(1-x-x^2)
fibs :: Stream Integer
fibs = gen (Series x / (1 - Series x - Series x * Series x))