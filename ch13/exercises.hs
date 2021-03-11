{- My exercises for chapter 13.  -}

import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

{- Q/A Exercises from page 499:
   1. (forever, when)
   2. Data.Bits, Database.Blacktip.Types
   3. Data types
   4. a.) Control.Concurrent, Control.Concurrent.MVar, Filesystem.Path.CurrentOS
      b.) Filesystem.Path.CurrentOS
      c.) Control.Monad
-}

maybeAll  :: (a -> Bool) -> Maybe a -> Bool
maybeAll  = all

eitherAll :: (a -> Bool) -> Either b a -> Bool
eitherAll = all
-- ^ Works because Maybe and Either implement Foldable.

-- Exercise: Modifying Code (page 525)
-- 2 & 3.
palindrome :: IO()
palindrome = forever $ do
    line1 <- getLine
    let line2 = map toLower $ filter isLetter line1
    case (line2 == reverse line2) of
        True  -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Nope!"
            exitSuccess
--

-- 4.
type Name = String
type Age  = Integer

data Person = Person Name Age
    deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
    deriving(Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == ""            = Left NameEmpty
    | not (age > 0)         = Left AgeTooLow
    | otherwise             = Left $ PersonInvalidUnknown $ 
                              "Name was: " ++ show name ++ 
                              " Age was: " ++ show age

gimmePerson :: IO()
gimmePerson = do 
    putStr "Enter Name: "
    name <- getLine
    putStr "Enter Age: "
    age  <- getLine
    let newPerson = mkPerson name (read age)
    case newPerson of
        (Right _)        -> putStrLn $ "Yay! Successfully got a Person: " ++ (show $
                            ((\(Right p) -> p) newPerson))
        (Left NameEmpty) -> error "Empty string should not be used for name."
        (Left AgeTooLow) -> error "Age must be greater than 0."
        (Left _ )        -> error "Unknown error with mkPerson"
--

