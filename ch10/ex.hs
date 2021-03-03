module Ex where
--author : vimal

-- folds => catamorphisms (from Category theory)
-- usually explained using lists, but is general concept
-- foldr and foldl; almost always we need foldr
-- folds happend in 2 stages; traversal and folding (reduction, but not always)
-- folds can return a list as a result (rebuilds list during folding stage)

-- simplified definition of foldr
-- not tail recursive

import Data.Time
import Data.List (sortBy)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

-- sum without using fold
sum' :: [Integer] -> Integer
sum' []       = 0
sum' (x : xs) = x + sum' xs

-- sum using folder
sum'' :: [Integer] -> Integer
sum'' xs = foldr (+) 0 xs

-- length without using fold
length' :: [a] -> Integer
length' []       = 0
length' (x : xs) = 1 + length' xs

-- length using fold
length'' :: [a] -> Integer
length'' xs = foldr (\_ y -> y + 1) 0 xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x y -> f x || y) False xs

-- myAny even [1..]
-- 1 `f` (foldr f False [2..])
-- 1 `f` (2 `f` (foldr f False [3..]))
-- 1 `f` True
-- True

-- simplified definition of foldl
-- tail recursive
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

-- Exercises : Understanding folds

-- 1. foldr (*) 1 [1..5]
-- ans : (b) and (c)

-- 2. Write out the evaluation steps for
-- foldl (flip (*)) 1 [1..3]
-- '*' is commutative so flip has no effect

-- foldl (*) (1*1) [2,3]
-- foldl (*) ((1*1)*2) [3]
-- foldl (*) (((1*1)*2)*3) []
-- (((1*1)*2)*3)
-- ((1*2)*3)
-- (2*3)
-- 3

-- 3. (c)

-- 4. (a)

-- 5.

-- (a) foldr (++) [] ["woot", "WOOT", "woot"]
-- (b) foldr max [] ["fear", "is", "the", "little", "death"]
-- (c) foldr (&&) True [False, True]
-- (d) foldr (||) True [False, True] => no, because True || x == True
-- (e) foldr ((++) . show) "" [1..5]
-- (f) foldr (flip const) 'a' [1..5]
-- (g) foldr (flip const) 0 "tacos"
-- (h) foldl const 0 "burritos"
-- (i) foldl const 'z' [1..5]

-- Exercises : Database processing

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

-- 1.

-- note here the order of the result is reversed because we use cons
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = go db []
  where  go [] acc = acc
         go ((DbDate d) : rest) acc = go rest (d : acc)
         go (_ : rest) acc = go rest acc

-- we can implement this using foldr (point free)

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = foldr (\i acc -> case i of
                                   DbDate d -> d : acc
                                   _ -> acc) []

-- 2.

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\i acc -> case i of
                                   DbNumber n -> n : acc
                                   _ -> acc) []

-- 3.

-- for sorting I referred :
-- https://ro-che.info/articles/2016-04-02-descending-sort-haskell
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = head . sortBy (flip compare) . filterDbDate

-- 4.

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sum nums)) / (fromIntegral (length nums))
           where
             nums = filterDbNumber db

-- Exercises : Scans Exercises

-- 1.
