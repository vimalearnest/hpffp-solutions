{- Q: "What are the odds of landing on the same monopoly space two revolutions in a 
    row?" 

   A:  -}

import Control.Monad (forever)
import System.Exit   (exitSuccess)
import Data.Char     (toLower)
import Data.Maybe    (isJust, isNothing)
import System.Random (randomRIO, Random)
import System.IO     (BufferMode(NoBuffering), hSetBuffering, stdout)

-- Some custom types this time, just to practice using custom types:
type Chance      = Double
type Roll        = Int
type SpaceNumber = Int
type Revolution  = Int

numSpaces = (40 :: SpaceNumber)

-- The percentage chance that an event will happen. In this case,
-- that a square on a monopoly board will be landed upon during consecutive
-- revolutions of the board by the same player.
getProbability :: Roll -> Roll -> Chance
getProbability _ 0 = 0.0
getProbability a b = rollToChance a / rollToChance b * 100

-- A model of the dice and their probabilities:
roll2d6 :: (Random a, Num a) => IO a
roll2d6 = do
    rollA <- randomRIO (1, 6)
    rollB <- randomRIO (1, 6)
    return $ (rollA + rollB)

rollToChance :: Roll -> Chance
rollToChance a = if   a < 0
                 then error "Rolls should never be negative."
                 else (sum $ replicate a 1.0)

allPossibleRolls :: [(Roll, Roll)]
allPossibleRolls = [(a, b) | a <- [1..6], b <- [1..6]]

allPossibleSums :: [Roll]
allPossibleSums  = map (\(a, b) -> a + b) allPossibleRolls

numOver :: Roll -> Roll
numOver a = length $ filter (> a)  allPossibleSums 

numOf   :: Roll -> Roll
numOf   a = length $ filter (== a) allPossibleSums

-- The chance to land on a given tile from within 2 - 12 spaces:
chanceToLandFrom :: Roll -> Chance
chanceToLandFrom a = (rollToChance $ numOf   a) / 36 * 100

-- The chance to overshoot a tile from within 2 - 12 spaces:
chanceToSkipFrom :: Roll -> Chance
chanceToSkipFrom a = (rollToChance $ numOver a) / 36 * 100
{- Chance to land from 7 spaces away is about 16.7%.
   Chances to land from 2 spaces or 12 spaces away is about 2.8%.
   Chance to overshoot/skip at 12 spaces away is 0%.
   Chance to overshoot/skip at 2 spaces away is about 97.2%.  -}

{- Then, the naive average chances to land on or overshoot a given tile from within 
   the 2 - 12 space window before it. The actual chances fluctuate but these are
   the average chances:  -}
naiveAverageChanceToLand :: Chance
naiveAverageChanceToLand = (foldr (\a -> (+ (chanceToLandFrom a))) 0 [2..12]) / 11
-- ^ About 9%
naiveAverageChanceToSkip :: Chance
naiveAverageChanceToSkip = (foldr (\a -> (+ (chanceToSkipFrom a))) 0 [2..12]) / 11
-- ^ About 45.5%

{- At this point I've got a simple model of the probabilities within dice-range
   of landing on the spot you want, or overshooting it. The way that I
   interpret this data is "If you are within 2 - 12 spaces of the target space
   then there is an average chance of almost 10% to land on the target space, 
   an average chance of almost 50% to not overshoot the target space, and
   probably a miniscule chance to miss the 2 - 12 space window entirely." This
   means that something slightly under naiveAverageChanceToLand is probably
   the answer in a universe where there are no second attempts per revolution.
   But, we see that there should be lots of second attempts per revolution,
   as naiveAverageChanceToSkip is about 45%. I was unsure how much this would
   wind up boosting the final result by in the long run, so I built a simulation 
   to test it.

   The result winds up being about a 14.28% - 14.29% chance to land on a given space
   for two consecutive revolutions. It would be neat to refine the model (above)
   based on the results of the simulation (below).  -}

{- I had the bright idea this time to use a single big record type as the
   data structure. I am very pleased with the way this wound up working for small
   inputs, but this actually winds up eating RAM more quickly than the data 
   structures I was using previously. My major Haskell goal for the near future is 
   to get the hang of idiomatically passing data structures like this through a 
   series of functions so that the garbage collector does its job correctly.  -}
data Sim = Sim
    { simSpaces          :: [Maybe Revolution]
    , simPlayer          :: SpaceNumber
    , simRevolution      :: Revolution
    , simEvents          :: Roll
    , simPotentialEvents :: Roll }

instance Show Sim where
    show (Sim a b c d e) = "After " ++ (show c) ++ " revolutions the"
        ++ " player has landed on space #" ++ (show b) ++ "." ++
        "\nThere were " ++ (show d) ++ " events out of " ++ (show e) ++
        " possible events." ++ "\nThe simulated probability is " ++
        show (getProbability d e) ++ "%"

-- A fresh simulation:
freshSim :: IO Sim
freshSim = return $ Sim (Just 0 : replicate (numSpaces - 1) Nothing) 0 0 0 0

-- This is another attempt as creating a function that represents a single
-- "step" in the simulation. They are then piped together below. I thought 
-- disassembling and re-assembling the data structure using pattern matching
-- was a clever idea and I am very happy with the result, but performance issues
-- remain and this is but an intermediate form of the eventual simulation.
simulateRoll :: Sim -> IO Sim
simulateRoll (Sim a b c d e) = do
    roll <- roll2d6
    let unwrap     = (\(Just f) -> f)
        landing    = (b + roll) `mod` numSpaces
        space      = a !! landing
        revolution = if landing < b     then c + 1 else c
        isEvent    = isJust space && revolution == (unwrap space) + 1
        events     = if isEvent         then d + 1 else d
        potentials = if isNothing space then e     else e + 1
        spaces     = (take landing a) ++ (Just revolution : (tail $ drop landing a))
    return $ Sim spaces landing revolution events potentials

-- I was very happy to get the turn-piping idea working the way I wanted,
-- but there is still a space complexity issue somewhere. It is as though the
-- simulation is keeping previous states around even after they've been passed
-- to the next simulateRoll. I have tried a few different ways to enforce strictness
-- but it is not quite there yet. 
runSimulation :: Roll -> IO Sim
runSimulation a = foldr (\_ -> (simulateRoll =<<)) freshSim [0..a]

-- Benchmark: 5,000,000 rolls in 0m25.309s; result of about 14.29%. Used way too 
-- much RAM due to my nascent understanding of the garbage collector, but it's 
-- getting there.
main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "Question: What are the odds of landing on the same space on a" ++
               " monopoly\nboard for consecutive revolutions?\n"
    forever $ do
        putStrLn $ "Note: sample sizes that are very low (such as 100,000) won't be"
                 ++ "super accurate." ++ "\nCaution: This program uses a lot of RAM."
                 ++ " inputs of over 3,000,000 are unwise for now."
        putStr "\nHow many rolls to simulate? "
        rolls <- getLine
        putStrLn $ "Simulating " ++ rolls ++ " dice rolls on the monopoly board..."
        results <- runSimulation (read rolls)
        putStrLn $ show results
        putStr "\nWould you like to run another simulation (y/Y to confirm)? "
        confirm <- getLine
        if   confirm /= "" && (toLower . head) confirm == 'y'
        then return ()
        else exitSuccess
 
