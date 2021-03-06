{- These are my attempts at the exercises in Chapter 12.  -}

import Data.List

checkTests :: [Bool] -> IO()
checkTests tests = do
    if   all (== True) tests
    then putStrLn "Test passed!"
    else putStrLn "Test failed!"

-- Exercise: String Processing (page 473):
-- 1.
notThe :: String -> Maybe String
notThe a = if a == "the" then Nothing else Just a

replaceThe :: String -> String
replaceThe "" = ""
replaceThe a = let e = concat $ map (\d -> d ++ " ") 
                              $ map (\b -> 
                                  case notThe b of
                                      Just c  -> c
                                      Nothing -> "a") $ words a
               in take (length e - 1) e

testReplaceThe :: IO()
testReplaceThe = do
    let
        tests = [ notThe "the"                  == Nothing
                , notThe "blahtheblah"          == Just "blahtheblah"
                , notThe "woot"                 == Just "woot"
                , replaceThe "the cow loves us" == "a cow loves us" ]

    checkTests tests

-- 2. 
isVowel :: Char -> Bool
isVowel a = if elem a "aeiouAEIOU" then True else False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel a = count (words a) 0
    where
        count :: [String] -> Integer -> Integer
        count []             b = b
        count (x : [])       b = b
        count (x : xs : xss) b = if   x == "the" && isVowel (head xs)
                                 then count xss (b + 1)
                                 else count xss b

testCountTheBeforeVowel :: IO()
testCountTheBeforeVowel = do
    let
        tests = [ countTheBeforeVowel "the cow"      == 0
                , countTheBeforeVowel "the evil cow" == 1 ] 

    checkTests tests

-- 3.
countVowels :: String -> Integer
countVowels xs = foldr (\b -> if isVowel b then (+ 1) else (+ 0)) 0 xs

testCountVowels :: IO()
testCountVowels = do
    let
        tests = [ countVowels "the cow"     == 2
                , countVowels "Mikolajczak" == 4 ]

    checkTests tests
--

-- Exercise: Validate the word (page 474):
newtype Word' = Word' String
    deriving(Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs = if   length (filter isVowel xs) <= length (filter (not . isVowel) xs)
            then Just (Word' xs)
            else Nothing

testMkWord :: IO()
testMkWord = do
    let
        tests = [ mkWord "shibby"  == Just (Word' "shibby")
                , mkWord "blibii"  == Just (Word' "blibii")
                , mkWord "iblibii" == Nothing ]
    
    checkTests tests
--

-- Exercise: It's only natural (page 475):
data Nat = Zero | Succ Nat
    deriving(Eq, Show)

natToInteger :: Nat -> Integer
natToInteger a = unwrap a 0
    where
        unwrap :: Nat -> Integer -> Integer
        unwrap Zero     c = c
        unwrap (Succ b) c = unwrap b (c + 1)

integerToNat :: Integer -> Maybe Nat
integerToNat a = if a >= 0 then Just (wrap a Zero) else Nothing
    where
        wrap :: Integer -> Nat -> Nat
        wrap 0 c = c
        wrap b c = wrap (b - 1) (Succ c)

testNat :: IO()
testNat = do
    let
        tests = [ natToInteger (Succ Zero)        == 1
                , natToInteger (Succ (Succ Zero)) == 2
                , integerToNat 0                  == Just Zero
                , integerToNat 1                  == Just (Succ Zero)
                , integerToNat 2                  == Just (Succ (Succ Zero))
                , integerToNat (-1)               == Nothing
                , natToInteger ((\(Just a) -> a) $ integerToNat 2) == 2 ]

    checkTests tests
--

-- Exercises: Small library for Maybe (page 476):
-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- 2. 
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing  = b
mayybee b f (Just a) = f a

testMayybee :: IO()
testMayybee = do
    let
        tests = [ mayybee 0 (+ 1) Nothing  == 0
                , mayybee 0 (+ 1) (Just 1) == 2 ]

    checkTests tests

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe a (Just b) = b

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes xs = map (\(Just a) -> a) $ filter isJust xs

testCatMaybes :: IO()
testCatMaybes = do
    let
        tests = [ catMaybes [Just 1, Nothing, Just 2] == [1, 2]
                , null (catMaybes $ take 3 $ repeat Nothing) ]

    checkTests tests

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if   any isNothing xs 
               then Nothing 
               else Just $ map (\(Just a) -> a) xs

testFlipMaybe :: IO()
testFlipMaybe = do
    let 
        tests = [ flipMaybe [Just 1, Just 2,  Just 3] == Just [1, 2, 3]
                , flipMaybe [Just 1, Nothing, Just 3] == Nothing ]

    checkTests tests
--

-- Exercises: Small library for Either (page 477):
-- 1. 
lefts' :: [Either a b] -> [a]
lefts' xs = foldr (\a -> case a of
                Left b -> (b :)
                _      -> id) [] xs

-- 2.
rights' :: [Either a b] -> [b]
rights' xs = foldr (\a -> case a of
                 Right b -> (b :)
                 _       -> id) [] xs

testRightsLefts :: IO()
testRightsLefts = do
    let
        sample = [Left 1, Left 2, Right 4, Right 6]
        tests  = [ lefts'  sample == [1, 2]
                 , rights' sample == [4, 6] ]

    checkTests tests

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right a) = Just (f a)
eitherMaybe' f _         = Nothing

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  a) = f a
either' _ f (Right a) = f a

-- 6. 
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f a = either' (\b -> Nothing) (\b -> Just $ f b) a

testEitherMaybe'' :: IO()
testEitherMaybe'' = do
    let
        tests = [ eitherMaybe'' isVowel (Right 'i') == Just True
                , eitherMaybe'' isVowel (Left  3)   == Nothing ]

    checkTests tests
--

-- Exercises: Unfolds (page 478):
-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ (myIterate f $ f a) 

testMyIterate :: IO()
testMyIterate = do
    let 
        tests = [ (take 5 $ myIterate (+ 1) 0)  == (take 5 $ iterate (+ 1) 0)
                , (take 5 $ myIterate (+ 1) 10) == (take 5 $ iterate (+ 1) 10)
                , (take 7 $ myIterate succ 'A') == "ABCDEFG" ]
    
    checkTests tests
   
-- 2. 
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
    Nothing     -> [] 
    Just (x, y) -> [x] ++ (myUnfoldr f y)

myIterate' :: (a -> a) -> a -> [a]
myIterate' f a = myUnfoldr (\b -> Just (b, f b)) a

testMyUnfoldr :: IO()
testMyUnfoldr = do
    let 
        tests = [ (take 5 $ myIterate' (+ 1) 0)  == (take 5 $ iterate (+ 1) 0)
                , (take 5 $ myIterate' (+ 1) 10) == (take 5 $ iterate (+ 1) 10)
                , (take 7 $ myIterate' succ 'A') == "ABCDEFG" ]
    
    checkTests tests
--

-- Exercise: Unfolding Binary Trees (page 481):
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving(Eq, Ord, Show)

-- 1. 
unfoldBtree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldBtree f x = case f x of
    Nothing        -> Leaf
    Just (a, b, c) -> Node (unfoldBtree f a) b (unfoldBtree f c)

-- 2. 
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldBtree (\a -> case a >= n of
    True  -> Nothing
    False -> Just (a + 1, a, a + 1)) 0

testTreeBuild :: IO()
testTreeBuild = do
    let
        tests = [ treeBuild 0 == Leaf
                , treeBuild 1 == Node Leaf 0 Leaf
                , treeBuild 2 == Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
                , treeBuild 3 == Node (Node (Node Leaf 2 Leaf)
                                            1
                                            (Node Leaf 2 Leaf))
                                      0
                                      (Node (Node Leaf 2 Leaf)
                                            1
                                            (Node Leaf 2 Leaf)) ]

    checkTests tests
--

