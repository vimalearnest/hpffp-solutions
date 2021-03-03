{- Exercises for Chapter 11 from the wonderful book: "Haskell Programming from 
   First Principles".   -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.List

data PugType              = PugData
data HuskyType a          = HuskyType
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a 
    deriving(Eq, Show)

-- Question/Answer exercises from page 394:  
--1. Type constructor 
--2. * -> *
--3. Doggies [Char]
--4. Num a => Doggies a
--5. Doggies Integer
--6. Doggies [Char]
--7. Trick question since they use the same name: Both.
--8. doge -> DogueDeBordeaux doge
--9. DogueDeBordeaux [Char]
--

data Price = Price Integer
    deriving(Eq, Show)

data Manufacturer = Mini | Mazda | Tata
    deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'us | TakeYourChancesUnited
    deriving (Eq, Show)

data PlaneSize = Regional | Jumbo 
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline PlaneSize
    deriving (Eq, Show)

myCar    = Car Mini  (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata  (Price 7000)
doge     = Plane PapuAir Regional

-- Exercises from page 398:

-- 1. What is the type of MyCar? Vehicle

-- 2. Defining some functions: 
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars xs = map (\x -> isCar x) xs

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car a _) = Just a
getManu _         = Nothing
-- 

{- Data Type Cardinality exercises from page 402:
    1. 1
    2. 3
    3. 2^16 == 65536
    4. Integer as a type does not seem to be bounded in the same way.
    5. Powers of 2. Int8 means an 8-bit integer, which is represented in binary
       at the lowest level; hence powers of 2.
-}

{- Exercises from page 403:
    1. Since MakeExample is a sort of function it has a function-like type
       signature: MakeExample :: Example. If you ask ghci for the type
       of example then you get an error.
    2. You get the information about type instances (Show) and the kind info.
    3. Because MakeExample is a function like a constructor it now shows the 
       type signature of a function that takes an int and returns an Example: 
       MakeExample' :: Int -> Example.
-}

-- Exercises: Pity the Bool (page 410):

-- 1: 2 + 2 = 4

-- 2: 256 + 2 = 258

{- The key for the above two is to remember that they are SUM types.
   The | in the data type description is like an addition operator for this
   purpose. -}
--

data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth
    deriving(Eq, Show)

data TwoQs = MkTwoQs QuantumBool QuantumBool
    deriving(Eq, Show)

-- Cardinality of TwoQs: 3 * 3 = 9
--

data Person = MkPerson String Int
    deriving(Eq, Show)
{- ^ The textbook says "The cardinality of this is frankly terrifying" because
   how you get a precise cardinality on a String is quite unclear!  -}

name :: Person -> String
name (MkPerson s _) = s

{- This is an example from the book explaining the difference between record
   syntax and normal product-type syntax. Record-types are just product
   types with a little syntactic sugar, such as built-in getters.  -}
data Person' = Person'
    { name' :: String
    , age'  :: Int
    } deriving(Eq, Show)

-- "Product types distribute over sum types." (page 415)
data Fiction = Fiction
    deriving Show

data Nonfiction = Nonfiction
    deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction
    deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)
    deriving Show

data Author' = Fiction' AuthorName | Nonfiction' AuthorName
    deriving(Eq, Show)

data Expr = 
      Number Int
    | Add Expr Expr
    | Minus Expr
    | Mult Expr Expr
    | Divide Expr Expr
{- ^ "This is in normal form, because it's a sum (type) of products:
   (Number Int) + Add (Expr Expr) + ... and so on." (page 416)  -}
--

-- Exercises: How does your garden grow? (page 417)
-- 1.
data FlowerType = Gardenia | Daisy | Rose | Lilac
    deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
    deriving Show

-- "Normal Form" as a "Sum of Products":
data Garden' = 
      Gardenia' Gardener
    | Daisy' Gardener
    | Rose' Gardener
    | Lilac' Gardener
    deriving Show
--

data OperatingSystem = 
      GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill  -- lol
    | Mac
    | Windows
    deriving(Eq, Show)

data ProgLang = 
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving(Eq, Show)

data Programmer = Programmer
    { os   :: OperatingSystem
    , lang :: ProgLang
    } deriving(Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- Exercise: Programmers (Page 426):
allProgrammers :: [Programmer]
allProgrammers = 
    [(Programmer a b) | a <- allOperatingSystems, b <- allLanguages]
{- ^ What makes this possible is that you can use the non-record product type
   syntax as shorthand for constructing a record type, because they are the same 
   thing.  -}
--

{- "In the arithmetic of calculating inhabitants of types, the function type
    is the exponent operator." (page 431)  -}

data Quantum = Yes | No | Both
    deriving(Eq, Show)


data Quad = One | Two | Three | Four
    deriving(Eq, Show)

-- Exercises: The Quad (page 434):
-- 1. 16?
-- 2. prodQuad: 4 * 4 = 16
-- 3. funcQuad: 4 ^ 4 = 256
-- 4. prodTBool: 2 * 2 * 2 = 2 ^ 3 = 8
-- 5. gTwo: 2 ^ 2 ^ 2 = 2 ^ (2 * 2) = 16
-- 6. fTwo: (2 ^ 4) ^ 4 = 65536
--

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving(Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)

-- Exercise: Binary Tree mapping exercise (page 442):
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)
--

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
    if mapTree (+ 1) testTree' == mapExpected
    then print "yup OK!"
    else error "test failed!"

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- Exercise: Converting binary trees to lists (page 444):
bigTestTree :: BinaryTree Integer
bigTestTree = insert' 6 $ insert' 5 $ insert' 4 testTree
{- ^ Using the example insert' function provided, it was helpful to create a 
     larger tree for testing. Because this tree is lop-sided it can reveal a
     need for more thorough pattern matching than would be needed for the 
     balanced trees above it.  -}

preorder :: BinaryTree a -> [a]
preorder (Node Leaf a Leaf)  = [a]
preorder (Node Leaf a r)     = concat [[a], preorder r]
preorder (Node l    a Leaf)  = concat [[a], preorder l]
preorder (Node l    a r)     = concat [[a], preorder l, preorder r]

inorder :: BinaryTree a -> [a]
inorder (Node Leaf a Leaf)   = [a]
inorder (Node Leaf a r)      = concat [[a], inorder r]
inorder (Node l    a Leaf)   = concat [inorder l, [a]]
inorder (Node l    a r)      = concat [inorder l, [a], inorder r]

postorder :: BinaryTree a -> [a]
postorder (Node Leaf a Leaf) = [a]
postorder (Node Leaf a r)    = concat [postorder r, [a]]
postorder (Node l    a Leaf) = concat [postorder l, [a]]
postorder (Node l    a r)    = concat [postorder l, postorder r, [a]]
--

testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "bad news bears."

testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "bad news bears."

testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"

testOrders :: IO()
testOrders = do
    testPreorder
    testInorder
    testPostorder

-- Exercise: Write foldr for a binary tree (page 445):
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f a btree = foldr f a $ inorder btree
--

-- Chapter Exercises (page 445):
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday
    deriving Show
-- 1. a.) Weekday is a type with five data constructors.
-- 2. c.) f :: Weekday -> String
-- 3. b.) Must begin with a capital letter.
-- 4. c.) Returns the final element of xs. 
--

-- "As-Pattern" exercises (447):
-- 1.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf a@(x : xs) (y : ys)
    | x == y && null xs = True
    | null ys           = False
    | x == y            = isSubseqOf xs ys
    | x /= y            = isSubseqOf a  ys

testSubseqOf :: IO()
testSubseqOf = do
    let
        tests = [ isSubseqOf "blah" "blahwoot" == True
                , isSubseqOf "blah" "wootblah" == True
                , isSubseqOf "blah" "wboloath" == True
                , isSubseqOf "blah" "wootbla"  == False
                , isSubseqOf "blah" "halbwoot" == False
                , isSubseqOf "blah" "blawhoot" == True ]
        
        testsPassed = all (== True) tests 

    if   testsPassed
    then putStrLn "Tests passed!"
    else putStrLn "Tests failed!"

-- 2. 
capitalizeWords :: String -> [(String, String)]
capitalizeWords a = map (\b@(x : xs) -> (b, ((toUpper x) : xs))) (words a)

testCapitalizeWords :: IO()
testCapitalizeWords = do
    let
        testString = "hello world"
        expected   = [("hello", "Hello"), ("world", "World")]
        testPassed = capitalizeWords testString == expected

    if testPassed
    then putStrLn "Test passed!"
    else putStrLn "Test failed!"

-- Exercise: Language Exercises (page 449):

--1. 
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs) = (toUpper x) : xs

--2.
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph (x : xs) = format (words ((toUpper x) : xs)) []
    where
        format :: [String] -> String -> String
        format (x : []) ys = ys ++ x
        format (x : xs : xss) ys
            | last x == '.' = format xss (ys ++ x ++ " " ++ ((capitalizeWord xs) ++ " "))
            | otherwise     = format xss (ys ++ x ++ " " ++ xs ++ " ")

testCapitalizeParagraph :: IO()
testCapitalizeParagraph = do
    let
        testString = "blah. woot ha."
        expected   = "Blah. Woot ha."
        testPassed = capitalizeParagraph testString == expected

    if   testPassed
    then putStrLn "Test passed!"
    else putStrLn "Test failed!"
--

-- Exercise: Phone exercise (page 450)
-- The next two exercises for this chapter are pretty meaty. This one
-- deserves a little consideration.

convo :: [String]
convo =
    [ "Wanna play 20 questions."
    , "Ya"
    , "U 1st haha"
    , "Lol OK. Have u ever tasted alcohol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "OK. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn" ]

data Button = Button
    { buttonLabel    :: Char
    , buttonContents :: [Char]
    } deriving Show

daPhone = [ Button '1' ['1']
          , Button '2' ['a', 'b', 'c', '2']
          , Button '3' ['d', 'e', 'f', '3']
          , Button '4' ['g', 'h', 'i', '4']
          , Button '5' ['j', 'k', 'l', '5']
          , Button '6' ['m', 'n', 'o', '6']
          , Button '7' ['p', 'q', 'r', 's', '7']
          , Button '8' ['t', 'u', 'v', '8']
          , Button '9' ['w', 'x', 'y', 'z', '9']
          , Button '0' ['0']
          , Button '#' ['.', ',', '#'] ]

type Digit   = Char
type Presses = Int

reverseTaps :: [Button] -> Char -> [(Digit, Presses)]
reverseTaps phone a
    | a == ' '  = [('0', 1)]
    | isUpper a = [('*', 1), (buttonLabel button, presses)]
    | otherwise = [(buttonLabel button, presses)]
    where
        button  = head $ filter (\x -> (elem (toLower a) (buttonContents x))) phone
        presses = 1 + (length $ takeWhile (\x -> x /= toLower a) (buttonContents button))

testReverseTaps :: IO()
testReverseTaps = do
    let
        testPass  = reverseTaps daPhone 'a' == [('2', 1)]
        testPass2 = reverseTaps daPhone 'A' == [('*', 1), ('2', 1)]

    if   testPass && testPass2
    then putStrLn "Test passed!"
    else putStrLn "Test failed!" 

reverseConvo :: [String] -> [Button] -> [[(Digit, Presses)]]
reverseConvo conv phone = map (reverseTaps phone) $ concat convo

-- 3. 
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = foldr (+) 0 $ map (\(a, b) -> b) xs

totalTaps :: [String] -> [Button] -> Presses
totalTaps conv phone = fingerTaps $ concat $ reverseConvo conv phone

-- 4.
extractConsec :: String -> String
extractConsec []       = []
extractConsec a@(x : xs) = takeWhile (\b -> b == x) a

mostPopularLetter :: String -> Maybe Char
mostPopularLetter [] = Nothing
mostPopularLetter xs = count (sort xs) []
    where
        count []       ys = Just (head ys)
        count a@(x : xs) ys 
            | (length $ extractConsec a) > length ys = count xs (extractConsec a)
            | otherwise                              = count xs ys

coolestLtr :: [String] -> Maybe Char
coolestLtr [] = Nothing
coolestLtr xs = mostPopularLetter $ concat xs

coolestLtr' :: [String] -> Maybe Char
coolestLtr' [] = Nothing
coolestLtr' xs
    | null deSpaced = Nothing
    | otherwise     = mostPopularLetter deSpaced
    where
        deSpaced = filter (\x -> x /= ' ') $ concat xs

extractConsec' :: [String] -> [String]
extractConsec' []       = []
extractConsec' a@(x : xs) = takeWhile (\b -> b == x) a

coolestWord :: [String] -> Maybe String
coolestWord [] = Nothing
coolestWord xs = count (sort $ concat $ map (\x -> words x) xs) []
    where
        count [] ys = Just (head ys)
        count a@(x : xs) ys
            | (length $ extractConsec' a) > length ys = count xs (extractConsec' a)
            | otherwise                               = count xs ys
--

-- Exercise: Hutton's Razor (page 452):
data HuttonExpr = HuttonLit Integer | HuttonAdd HuttonExpr HuttonExpr
-- 1.
evalHutton :: HuttonExpr -> Integer
evalHutton (HuttonLit a)   = a
evalHutton (HuttonAdd a b) = (evalHutton a) + (evalHutton b)

testEvalHutton :: IO()
testEvalHutton = do
    let
        testPassed = evalHutton (HuttonAdd (HuttonLit 1) (HuttonLit 9001)) == 9002

    if   testPassed
    then putStrLn "Test passed!"
    else putStrLn "Test failed!"

-- 2. 
intToStr :: Int -> String
intToStr a = convert a [] ""
    where
        convert :: Int -> String -> String -> String
        convert a xs id
            | a < 0     = convert (a * (- 1)) xs "-"
            | a < 10    = id ++ ((intToDigit a) : xs)
            | otherwise = convert (quot a 10) ((intToDigit $ rem a 10) : xs) id

printExpr :: HuttonExpr -> String
printExpr (HuttonLit a)   = intToStr (fromInteger a)
printExpr (HuttonAdd a b) = (printExpr a) ++ " + " ++ (printExpr b)

testPrintExpr :: IO()
testPrintExpr = do
    let 
        testPassed  = printExpr (HuttonAdd (HuttonLit 1) (HuttonLit 9001)) == "1 + 9001"
        a1          = HuttonAdd (HuttonLit 9001) (HuttonLit 1)
        a2          = HuttonAdd a1               (HuttonLit 20001)
        a3          = HuttonAdd (HuttonLit 1)    a2
        testPassed2 = printExpr a3 == "1 + 9001 + 1 + 20001"

    if   testPassed && testPassed2
    then putStrLn "Test passed!"
    else putStrLn "Test failed!"
--

