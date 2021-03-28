{- Q: "What are the odds of landing on the same monopoly space two revolutions in a 
    row?" 

   A:  -}

{- Note: Choosing to do even a small part of a Haskell project using unbounded 
   Integers instead of using Ints leads to a little extra boilerplate. This is because
   a lot of the base Haskell functions (such as take/drop/length) either require an Int
   as an argument or return an Int as a value. It is my understanding that the reason
   there are both Int and Integer type classes is that it is slightly more 
   computationally expensive to use Integers over Ints. In this instance, I 
   implemented my Fraction module using Integer and chose to represent the dice
   rolls as Integers. This required writing toInt and integerLength as
   convenience functions to translate between the two ways of representing
   integer values. I have not noticed a decrease in performance for using
   Integers.  -}

import System.Random (randomRs, mkStdGen)
import Data.List     (foldl')
import Control.Monad (forever)
import System.Exit   (exitSuccess)
import Data.Char     (toLower)
import Data.Maybe    (isNothing, isJust)
import System.IO     (BufferMode(NoBuffering), hSetBuffering, stdout)
import Fraction

type Seed   = Int
type Chance = Fraction

numSpaces = 40 :: Integer

percentageChance :: Chance -> Double
percentageChance a@(Fraction _ d) =
    if   d == 0
    then 0
    else (fractionToDouble a) * 100

-- (Integer <---> Int) convenience functions:
integerLength :: [a] -> Integer
integerLength xs = foldr (\_ -> (+ 1)) 0 xs

toInt :: (Num a, Enum a) => a -> Int
toInt a = foldr (\_ -> (+ 1)) 0 [1..a]

-- Modeling the dice:
d6            = [1..6]                     :: [Integer]
diceRange2d6  = [2..12]                    :: [Integer]
diceRangeSize = integerLength diceRange2d6 :: Integer

-- All possible outcomes of 2d6:
allPossibleRolls = [(a, b) | a <- d6, b <- d6]             :: [(Integer, Integer)]
allPossibleSums  = map (\(a, b) -> a + b) allPossibleRolls :: [Integer]
numPossibleRolls = integerLength allPossibleRolls          :: Integer

{- By counting how many elements of a list are equal to, greater than,
   or less than the result of a 2d6 roll, we can figure out what the
   odds of landing on, over-shooting, or under-shooting the desired
   space is:  -}
numOf :: Eq a => a -> [a] -> Integer
numOf a xs = integerLength $ filter (== a) xs

numOver :: Ord a => a -> [a] -> Integer
numOver a xs = integerLength $ filter (> a) xs

numUnder :: Ord a => a -> [a] -> Integer
numUnder a xs = integerLength $ filter (< a) xs

chanceToLandFrom :: Integer -> Chance
chanceToLandFrom a = Fraction (numOf a allPossibleSums) numPossibleRolls

chanceToSkipFrom :: Integer -> Chance
chanceToSkipFrom a = Fraction (numOver a allPossibleSums) numPossibleRolls

chanceToLandShortFrom :: Integer -> Chance
chanceToLandShortFrom a = Fraction (numUnder a allPossibleSums) numPossibleRolls

{- By mapping the above functions on to the effective range of a 2d6 dice roll,
   summing the result, and dividing it by the size of the 2d6 window, we can get the
   average chance to land on, skip, or over-shoot the target space while within the
   2d6 window of a desired space:  -}
avgChanceToLand :: Chance
avgChanceToLand = 
    let a = sum [chanceToLandFrom a | a <- diceRange2d6]
    in  fracDiv a (Fraction diceRangeSize 1)
-- ^ 1/11 or about 9.09%

avgChanceToSkip :: Chance
avgChanceToSkip = 
    let a = sum [chanceToSkipFrom a | a <- diceRange2d6]
    in  fracDiv a (Fraction diceRangeSize 1)
-- ^ 5/11 or about 45.45%

avgChanceToLandShort :: Chance
avgChanceToLandShort = 
    let a = sum [chanceToLandShortFrom a | a <- diceRange2d6]
    in  fracDiv a (Fraction diceRangeSize 1)
-- ^ 5/11 or about 45.45%

{- Although the rest of it remains to be sorted out, I think it is safe to
   infer from this so far that the chance of landing on a space for consecutive
   revolutions of the board should be above 9%. This is because avgChanceToLandShort
   is 45.45%. Nearly half the time (on average) you will get more than one chance to
   try and reach a space per revolution. Rarely, the player might miss the window to
   land on a space by first landing 13 spaces before it, and then rolling a 12.
   Running a simulation seems to suggest that the actual result is about 14.29%,
   which seems about right. I plan on finishing the model above so that it comes to
   the complete result without needing to run a simulation. On to the sim:  -}
--------------------------------------------------------------------------------

{- Using the non-monadic random number generator from the random 1.1 module on
   stackage, as opposed to the monadic one from the last version, I have here
   taken the desired amount of rolls from an infinite list of values generated
   within the range [1..6]:  -}
dice :: Seed -> Int -> [Integer]
dice s c = take c $ randomRs (1, 6) (mkStdGen s)

{- I decided to use two seeds for the dice here, but have the second seed be
   determined by the first seed:  -}
rolls :: Seed -> Int -> [Integer]
rolls s c = zipWith (+) (dice s c) (dice (s + 1) c)

{- By folding over allRolls we could find the final landing space, but by
   scanning over allRolls we can find *all* landing spaces:   -}
landings :: [Integer] -> [Integer]
landings xs = scanl (\a b -> (a + b) `mod` numSpaces) 0 xs

{- This leaves us with a cyclic list of landing spaces mod 40. But, what we really
   want is a list of lists, with each list containing a single revolution's
   worth of spaces. Since you can do almost anything with a fold as long as your
   anonymous function matches the signature required by the fold, it is possible
   to arbitrarily re-organize a list by strictly increasing segments. The resulting 
   list of lists is reversed, but that doesn't matter as an intermediate step:  -}
reverseChunkByIncreasing :: [Integer] -> [[Integer]]
reverseChunkByIncreasing xs = foldl' (\b a ->
    if   (head . head) b < a 
    then ((a : (head b)) : (tail b))
    else [a] : b) [[head xs]] (tail xs)

{- Using the reverse-chunked [[Integer]] generated by the above function, it is 
   possible to find the chance of the event happening by folding over the list and 
   comparing each "chunk" (except the first) with the previous chunk. The number of 
   potential events is the number of elements in all of the chunks minus the one 
   representing the first revolution, while the number of events is the number of 
   elements in each chunk (after the one representing the first) which also appear 
   in the previous chunk:  -}
findChance :: Seed -> Int -> Chance
findChance s c =
    let r = (reverseChunkByIncreasing . landings) $ rolls s c
        f = (\(Fraction n d) -> Fraction n (d - (integerLength $ last r))) 
    in  f . fst $ foldr (\a ((Fraction n d), ys) ->
        let events     = integerLength $ filter (`elem` a) ys
            potentials = integerLength ys
        in  ((Fraction (n + events) (d + potentials)), a)) ((Fraction 0 0), []) r

confirm :: String -> Bool
confirm xs = xs /= "" && (toLower . head) xs == 'y'

{- Performance Note: There is still a minor space complexity issue. Here are some
   approximate performance results for various inputs on my machine:

     Input Size | RAM use | Time
     -----------+---------+------
     500,000    | 128MB   | 0.9 seconds
     5,000,000  | 500MB   | 5.5 seconds   
     25,000,000 | 2GB     | 23  seconds
     50,000,000 | 4GB     | 48  seconds
       --^(all using seed: 1988)^--

    This is O(n) in Space and O(n) in time. I would like it to be O(1) in Space and
    O(n) in time, which is what I would expect that idiomatic code would provide in
    this instance. The culprits are almost certainly unevaluated thunks that remain
    to be rooted out. I am pretty sure it is possible to do so and will keep at it,
    as I think that Haskell is a neat language.

    Note that an input of 500,000 is more than sufficient to determine an accurate 
    result, so this is mostly a philosophical question. It seems like the cases where
    lazy evaluation are most interesting are also the ones that can cause runaway
    space complexity. All in all I think it is a pretty good way to cap off chapters
    1 - 13, and I am pretty happy with it. I will revisit this occasionally to try
    and get slightly better space complexity out of it. Anyone who wants to have a 
    go themselves is more than welcome, as it is always possible I missed a better
    way.  -}

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    putStrLn $ "Question: What are the odds of landing on the same space on a" ++
               " monopoly\nboard for consecutive revolutions?\n"
    forever $ do
        putStrLn $ "Note: sample sizes that are very low (such as 100,000) won't be"
                 ++ "super accurate." ++ "\nCaution: This program uses a lot of RAM."
                 ++ " inputs of over 3,000,000 are unwise for now."
        putStr "\nHow many rolls to simulate? "
        numRolls <- getLine
        putStr "\nEnter a seed for the simulation: "
        seed     <- getLine
        putStrLn $ "Simulating " ++ numRolls ++ " dice rolls on the monopoly board..."
        let result = findChance (read seed) (read numRolls)
        putStrLn $ "events / potential events: " ++ (show result) ++ " " ++
                   "or " ++ (show . percentageChance $ result) ++ "%"
        putStr "\nWould you like to run another simulation (y/Y to confirm)? "
        again <- getLine
        if   confirm again
        then return ()
        else exitSuccess

