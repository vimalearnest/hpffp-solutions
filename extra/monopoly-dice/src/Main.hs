{- Q: "What are the odds of landing on the same monopoly space two revolutions in a 
    row?" 

   A:  -}

import Data.Char     (toLower)
import System.Random (randomRIO)
import System.IO     (BufferMode(NoBuffering),
                      hSetBuffering,
                      stdout)

-- First, model the dice:
allPossibleRolls = [(a, b) | a <- [1..6], b <- [1..6]]
allPossibleSums  = map (\(a, b) -> a + b) allPossibleRolls

-- The chance to land on a given tile from within 2 - 12 spaces:
chanceToLandFrom :: Double -> Double
chanceToLandFrom a = (hits / 36) * 100
    where
        hits = foldr (\b -> case b == a of 
            True  -> (+ 1) 
            False -> id) 0 allPossibleSums

-- Chance to land from 7 spaces away is about 16.7%.
-- Chances to land from 2 spaces or 12 spaces away is about 2.8%.

-- The chance to overshoot a tile from within 2 - 12 spaces:
chanceToSkipFrom :: Double -> Double
chanceToSkipFrom a = (skips / 36) * 100
    where
        skips = foldr (\b -> case b > a of 
            True  -> (+ 1) 
            False -> id) 0 allPossibleSums

-- Chance to overshoot/skip at 12 spaces away is 0%.
-- Chance to overshoot/skip at 2 spaces away is about 97.2%.

{- The naive average chances to land on or overshoot a given tile from within the
   2 - 12 space window before it. The actual chances fluctuate but these are
   the average chances.  -}
naiveAverageChanceToLand = (foldr (\a -> (+ (chanceToLandFrom a))) 0 [2..12]) / 11
-- ^ About 9%
naiveAverageChanceToSkip = (foldr (\a -> (+ (chanceToSkipFrom a))) 0 [2..12]) / 11
-- ^ About 45.5%

{- At this point I've got a simple model of the probabilities within dice-range
   of landing on the spot you want, or overshooting it. The way that I
   interpret this data is "If you are within 2 - 12 spaces of the target space
   then there is an average chance of almost 10% to land on the target space, 
   an average chance of almost 50% to not overshoot the target space, and
   probably a miniscule chance to miss the 2 - 12 space window entirely." This
   means that something slightly under naiveAverageChanceToLand is the probably
   the answer in a universe where there are no second attempts per revolution.
   But, we see that there should be lots of second attempts per revolution,
   as naiveAverageChanceToSkip is over 45%. I was unsure how much this would
   wind up boosting the final result by, so I built a simulation to test it.

   The result winds up being about a 14.28% chance to land on a given space
   for two consecutive revolutions. It would be neat to refine the model (above)
   based on the results of the simulation (below).  -}

-- The board:
data Space = Space
    { spaceNumber :: Int
    , spaceLanded :: [Int]
    } deriving(Show)

data Board  = Board
    { boardSpaces     :: [Space] 
    , boardPlayerAt   :: Int
    , boardRevolution :: Int }

instance Show Board where
    show (Board xs p t) = (concat $ map (\a -> (show a) ++ "\n") xs) ++
                          ("Player on space # " ++ (show p)) ++
                          ("\nTurn # " ++ (show t))

markRevolution :: Board -> Board
markRevolution (Board xs p t) = case noError of
    True  -> Board insertChoice p t
    False -> error "Bad input or duplicate space labels."
    where
        noError      = length choice == 1
        choice       = filter (\(Space a _) -> a == p) xs
        noChoice     = filter (\(Space a _) -> a /= p) xs
        insertChoice = (((\(Space a ys) -> (Space a (t : ys))) (head $ choice)) : noChoice)

-- The starting board. Marking the first spot is optional, and I have
-- chosen to do so here since it seems like we might as well include it.
board = markRevolution $ Board [Space a [] | a <- [0..39]] 0 0

-- This simulates a single turn and marks the spot on which the player landed.
-- This seemed like the ideal part of the problem to turn into some kind of
-- monad. The idea was to then pipe all of these turns together. It works,
-- but not perfectly.
oneTurn :: Board -> IO Board
oneTurn (Board xs p t) = do
    diceA <- randomRIO (1, 6)
    diceB <- randomRIO (1, 6)
    let newSpot    = mod (diceA + diceB + p) 40
        revolution = if newSpot < p then t + 1 else t
    return $ markRevolution (Board xs newSpot revolution)

-- This is, in effect, piping monads. It can probably be expressed with
-- a foldM or foldM_, and/or replicateM and replicateM_. A near-term Haskell goal
-- is to know how to do that without direct recursion. 
-- I was very happy to get this working the way I wanted it to work, but there 
-- is a performance bottleneck somewhere and it could be here.
manyTurns :: Int -> Board -> IO Board
manyTurns n b = if   n > 0 
                then simulate (n - 1) (oneTurn b)
                else error "Must enter value greater than 0 for manyTurns."
    where
        simulate :: Int -> IO Board -> IO Board
        simulate 0 b = b
        simulate a b = simulate (a - 1) (b >>= oneTurn)

-- An Event is described here as a space that has been visited on
-- two consecutive revolutions. The revolutions are stored in the
-- Space as a list of Ints. The number of events is then
-- returned as an Int.
getEvents :: [Int] -> Int
getEvents []       = 0
getEvents (x : xs) = parse x xs 0
    where
        parse :: Int -> [Int] -> Int -> Int
        parse _ []       n = n
        parse b (x : xs) n = if   (b - 1) == x
                             then parse x xs (n + 1)
                             else parse x xs n

-- Now, the number of Events from each Space and sum up the result.
getAllEvents :: Board -> Int
getAllEvents (Board xs _ _) = sum $ map (\(Space a ys) -> getEvents ys) xs

-- Rolls during the 0th revolution do not count as a potential Event, as no Event 
-- could have occured then.
getPotentialEvents :: Board -> Int
getPotentialEvents (Board xs _ _) = foldr (\x -> (+ (length $ filter (/= 0) $ spaceLanded x))) 0 xs

intToDouble :: Int -> Double
intToDouble n = if n > 0 
                then sum $ replicate n 1.0
                else sum $ replicate n (- 1.0)

{------------------------------------------------------------------------------
   Simulation Output: (for 20,000,000 rolls of the dice)
    > How many rolls to simulate? 20000000
    > Simulating 20000000000 dice rolls on the monopoly board...
    > Simulation complete! Calculating results...
    > There were 2857238.0 events out of 1.9999994e7 possible events.
    > The simulated probability of the event occuring is: 14.286194285858286
    > Would you like to view the final board state (y/Y to confirm)? n
    > Exiting program. 
------------------------------------------------------------------------------}
-- Note: Do not view board state unless it's a very small simulation. It is
--       for observing how the data structure works using small inputs.
main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "Caution: Input very large numbers (> 20,000,000) risks a crash.\n"
             ++ "Any number (> 200,000) should be a sufficient sample size."
    putStr "How many rolls to simulate? "
    rolls  <- getLine
    putStrLn $ "Simulating " ++ rolls ++ " dice rolls on the monopoly board..."
    result <- manyTurns (read rolls) board
    putStrLn $ "Simulation complete! Calculating results..."
    let numEvents      = intToDouble $ getAllEvents       result
        possibleEvents = intToDouble $ getPotentialEvents result
        probability    = (numEvents / possibleEvents) * 100
    putStrLn $ "There were " ++ (show numEvents) ++ " events out of " ++ 
               (show possibleEvents) ++ " possible events."
    putStrLn $ "The simulated probability of the event occuring is: " ++ 
               (show probability)
    putStr "Would you like to view the final Board state? (y/Y to confirm)? "
    confirm <- getLine
    if   (not . null) confirm && (toLower $ head confirm) == 'y'
    then putStrLn $ show $ result
    else putStrLn "Exiting program."

