{- This wonderful book has a recurring series of exercises where it uses lessons
   learned to ask the reader to implement ciphers. These are my attempts at those
   exercises so far.  -}

module Cipher where

import Data.Char

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
    let 
        testString   = "zZbCd"
        testString2  = "aAcDe"
        testPassed   = (caesar 1 testString)               == testString2
        testPassed2  = (caesar (-1) $ caesar 1 testString) == testString

    if   testPassed && testPassed2
    then putStrLn "Test passed!"
    else putStrLn "Test failed!"
{- ^ Note: In order to "unCaesar", just pass it the opposite amount of shift
           that you used to encipher it.  -}
--

deSpace :: String -> String
deSpace text = concat $ words text

enSpace :: String -> String -> String
enSpace deSpaced original = format deSpaced (words original) []
    where
        format :: String -> [String] -> String -> String
        format [] ys       zs = take (length zs - 1) zs
        format xs (y : ys) zs = format (drop (length y) xs)
                                       ys
                                       (zs ++ ((take (length y) xs) ++ " "))

-- Chapter 11 asks the reader to implement a Vigenere cipher:
vigenere :: String -> String -> String
vigenere key text = enSpace enciphered text
    where
        fullKey    = take (length $ deSpace text) $ cycle key
        paired     = zip fullKey $ deSpace text
        enciphered = [shiftLetter (letterToInt $ fst c) (snd c) | c <- paired]

unVigenere :: String -> String -> String
unVigenere key text = enSpace deciphered text
    where
        fullKey    = take (length $ deSpace text) $ cycle key
        paired     = zip fullKey $ deSpace text
        deciphered = [shiftLetter (- (letterToInt $ fst c)) (snd c) | c <- paired]
{- ^ Note: As described in the textbook, the repeating key does not consider
     the spaces in the original plaintext. To get the exact same result as the
     book expects, you need to remove the spaces before enciphering and then
     put them back in afterwards.  -}

testVigenere :: IO()
testVigenere = do
    let 
        testString  = "Meet at dawn"
        cipherKey   = "ALLY"
        testString2 = "Mppr ae oywy"
        enciphered  = vigenere cipherKey testString
        testPassed  = enciphered == testString2
        testPassed2 = unVigenere cipherKey enciphered == testString

    if   testPassed && testPassed2
    then putStrLn "Test passed!"
    else putStrLn "Test failed!"
--

