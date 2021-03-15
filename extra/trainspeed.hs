{- The Problem: Train A is going 137mph, and Train B is going 54 miles per hour.
   Train A takes 7.5 seconds to overtake Train B. How long in feet is Train A? 
   This problem was put forth by a YouTube channel involving Shinkansen trains.
  
   Although this is a pretty short snippet, I had a lot of fun writing it and
   feel like the language lends itself to this kind of thing. Feel free to correct
   my math if I got the result wrong. I am mostly confident in it.  -}

-- Data structure representing speed units:
data Speed  = FPS Double | MPH Double
    deriving(Eq, Show)

-- Data structure representing a Train, which just wraps a Speed.
data Train  = Train Speed
    deriving(Eq, Show)

-- Conversion factors:
secsPerHour :: Double
secsPerHour = 3600

feetPerMile :: Double
feetPerMile = 5280

-- Converts Feet/Second <--> Miles/Hour
convertSpeed :: Speed -> Speed
convertSpeed (FPS a) = MPH ((a * secsPerHour) / feetPerMile)
convertSpeed (MPH a) = FPS ((a * feetPerMile) / secsPerHour)

-- Convenience functions for using a Speed in an expression:
speedInFPS :: Speed -> Double
speedInFPS a = case a of
    (FPS b) -> b
    (MPH b) -> ((\(FPS c) -> c) $ convertSpeed a)

speedInMPH :: Speed -> Double
speedInMPH a = case a of
    (FPS b) -> ((\(MPH c) -> c) $ convertSpeed a)
    (MPH b) -> b

-- The trains themselves
trainA = Train (MPH 137)
trainB = Train (MPH 54)

-- Get the speed of one train relative to another in feet/second:
relativeSpeedInFPS :: Train -> Train -> Double
relativeSpeedInFPS (Train a) (Train b) = (speedInFPS a) - (speedInFPS b)

-- How many feet/second train A is moving minus the speed of train B:
aRelativeToB = relativeSpeedInFPS trainA trainB

-- How many feet of Train A will have passed train B during the 7.5 seconds:
passedLengthInFeet = aRelativeToB * 7.5

-- The answer is 913 feet. 

