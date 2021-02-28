module ChEx where

stops  = "pbtdkg"
vowels = "aeiou"

-- Warmp-up and review
-- 1.

-- (a)
words1 =  [(s1,v,s2) | s1 <- stops, s2 <- stops, v <- vowels]

-- (b)
words2 =  [(s1,v,s2) | s1 <- stops, s2 <- stops, v <- vowels, s1 == 'p']

nouns = ["cat", "fish", "ball", "lake"]
verbs = ["eats", "swims", "plays"]

-- (c)
sentences = [(n1, v, n2) | n1 <- nouns, n2 <- nouns, v <- verbs]

-------------------

-- 2.

seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- The function above returns the average length of words
-- in the given text

-- λ> seekritFunc "big bad wolf jumps over the fence"
-- 3
-- which is 'div 27 8' where 8 is the number of the words and
-- 27 is the sum of lengths of all the words.

-- 3.

seekritFunc1 x =
   (/) (fromIntegral charCount) (fromIntegral wordCount)
   where charCount = (sum (map length (words x)))
         wordCount = (length (words x))

-- Rewriting functions using folds

-- 1.

myOr :: [Bool] -> Bool
myOr = undefined

-- 2.

myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined

-- 3.

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 = undefined

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 = undefined

-- 4.

myReverse :: [a] -> [a]
myReverse = undefined

-- 5.

myMap :: (a -> b) -> [a] -> [b]
myMap = undefined

-- 6.

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined

-- 7.

squish :: [[a]] -> [a]
squish = undefined

-- 8.

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined

-- 9.

squishAgain :: [[a]] -> [a]
squishAgain = undefined

-- 10.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined


-- 11.

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined
