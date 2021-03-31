{- This wonderful book has a recurring series of exercises where it uses lessons
   learned to ask the reader to implement ciphers. These are my attempts at those
   exercises so far.  -}

module Cipher where

import Data.Char -- to-do, encapsulate
import Data.List (foldl')

checkTests :: [Bool] -> IO()
checkTests xs = if   all (== True) xs
                then putStrLn "Test passed!"
                else putStrLn "Test failed!"

letterToInt :: Char -> Int
letterToInt a  = length $ takeWhile (\x -> x /= toLower a) ['a'..'z']

intToLetter :: Int -> Char
intToLetter  a = ['a'..'z'] !! a

intToLetter' :: Int -> Char
intToLetter' a = ['A'..'Z'] !! a

shiftLetter :: Int -> Char -> Char
shiftLetter a b
    | isLower b = intToLetter  $ mod (letterToInt b + a) 26
    | isUpper b = intToLetter' $ mod (letterToInt b + a) 26
    | otherwise = b

-- Chapter 9 asks the reader to implement a Caesar cipher:
caesar :: Int -> String -> String
caesar shift text = map (shiftLetter shift) text

testCaesar :: IO()
testCaesar = do
    let testString   = "zZbCd"
        testString2  = "aAcDe"
        tests = [ (caesar 1 testString)               == testString2
                , (caesar (-1) $ caesar 1 testString) == testString ]

    checkTests tests
{- ^ Note: In order to "unCaesar", just pass it the opposite amount of shift
           that you used to encipher it.  -}

-- This is a pretty naive way to handle negative numbers, for now.
isNegative :: String -> Bool
isNegative [] = False
isNegative xs = '-' `elem` xs

stringToInt :: String -> Int
stringToInt xs = (\a -> if isNegative xs then a * (-1) else a) $
    foldl' (\b a -> 
        if isDigit a 
        then ((b * 10) + digitToInt a) 
        else b) 0 xs

-- Chapter 13 asks the reader to implement an interactive version of this cipher:
iCaesar :: IO()
iCaesar = do
    putStr "Enter Text: "
    text  <- getLine
    putStr "Enter Shift: "
    shift <- getLine 
    putStrLn $ "Enciphered: " ++ (caesar (stringToInt shift) text) 
--

removeNonLetters :: String -> String
removeNonLetters text = filter isLetter text

returnNonLetters :: String -> String -> String
returnNonLetters text original = fst $
    foldr (\a (b, c@(x : xs)) ->
        if   not . isLetter $ a
        then ((a : b), c)
        else ((x : b), xs)) ([], reverse text) original

-- Chapter 11 asks the reader to implement a Vigenere cipher:
vigenere :: String -> String -> String
vigenere key text   = returnNonLetters enciphered text
    where
        parsed      = removeNonLetters text
        fullKey     = take (length parsed) $ cycle $ removeNonLetters key
        paired      = zip fullKey parsed
        enciphered  = [shiftLetter (letterToInt $ fst c) (snd c) | c <- paired]

unVigenere :: String -> String -> String
unVigenere key text = returnNonLetters deciphered text
    where
        parsed      = removeNonLetters text
        fullKey     = take (length parsed) $ cycle $ removeNonLetters key
        paired      = zip fullKey parsed
        deciphered  = [shiftLetter (- (letterToInt $ fst c)) (snd c) | c <- paired]
     {- To account for errors during interactive input and match the textbook's
        results exactly, this function removes all non-letters before
        enciphering/deciphering and then it re-inserts them afterwards in
        the same positions.  -}

testVigenere :: IO()
testVigenere = do
    let 
        testString  = "Meet at dawn"
        cipherKey   = "ALLY"
        testString2 = "Mppr ae oywy"
        testString3 = "Meet, at dawn?"
        cipherKey2  = "Ally!"
        testString4 = "Mppr, ae oywy?"
        enciphered  = vigenere cipherKey  testString
        enciphered2 = vigenere cipherKey2 testString3
        tests = [ enciphered                        == testString2
                , unVigenere cipherKey enciphered   == testString
                , enciphered2                       == testString4
                , unVigenere cipherKey2 enciphered2 == testString3 ]

    checkTests tests
--

-- Chapter 13 asks the user to implement an interactive version of this cipher:
iVigenere :: IO()
iVigenere = do
    putStr "Enter Text: "
    text <- getLine
    putStr "Enter Key: "
    key  <- getLine
    putStr "(E)ncrypt or (D)ecrypt? "
    op   <- getLine
    if null op then putStrLn "Invalid input!" else
        case (toLower $ head op) of
            'e' -> putStrLn $ "Enciphered: " ++ (vigenere   key text)
            'd' -> putStrLn $ "Deciphered: " ++ (unVigenere key text)
            _   -> putStrLn "Invalid input!"
--

