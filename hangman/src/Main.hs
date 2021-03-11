{- This is the hangman interactive tutorial project from the Haskell Book,
   starting at chapter 13. Some of it was left as an exercise to the reader,
   and I have done those exercises here. I have also improved on it in some of 
   the ways suggested in the chapter.  -}

module Main where

import Control.Monad (forever)
import Data.Maybe    (isJust)
import Data.List     (intersperse)
import System.Exit   (exitSuccess)
import System.Random (randomRIO)
import Data.Char     (toLower,
                      isLetter)
import System.IO     (BufferMode(NoBuffering),
                      hSetBuffering,
                      stdout)

newtype WordList = WordList [String]
    deriving(Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

guessLimit :: String -> Int
guessLimit w = (div (length w) 2) + length w

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter allLetters $ filter gameLength aw)
    where 
        allLetters w = all isLetter w

        gameLength w = let l = length (w :: String)
                       in  l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
    show (Puzzle _ discovered guessed _) = 
        (intersperse ' ' $ fmap renderPuzzleChar discovered) 
        ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (\a -> Nothing) s) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord p a = elem a ((\(Puzzle s _ _ _) -> s) p)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed p a = elem a ((\(Puzzle _ _ g _) -> g) p)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'  
renderPuzzleChar (Just a) = a

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter a@(Puzzle word filledInSoFar s _) c = Puzzle word newFilledInSoFar (c : s) ((\(Puzzle _ _ _ b) -> b) a)
    where
        zipper guessed wordChar guessChar = if   wordChar == guessed
                                            then Just wordChar
                                            else guessChar
        
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

incPenalty :: Puzzle -> Puzzle
incPenalty (Puzzle a b c d) = Puzzle a b c (d + 1)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True)  -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _)  -> do
            putStrLn "This character was in the word, filling in the word accordingly."
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            return $ incPenalty $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ tries) = 
    if   tries >= guessLimit wordToGuess
    then do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess
    else do
        putStrLn $ "Bad Guesses: " ++ (show tries) ++ "/" ++ (show $ guessLimit wordToGuess)

gameWin :: Puzzle -> IO()
gameWin (Puzzle word filledInSoFar _ _) =
    if   all isJust filledInSoFar
    then do
        putStrLn $ "You win! The word was: " ++ word
        exitSuccess
    else return ()

runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character!"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

