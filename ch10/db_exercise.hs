{- Database exercise from Chapter 10.  -}

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime
    deriving(Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)) ]
   
-- Exercises on the "database" from page 371:
-- 1. 
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = map (\x -> getTime x) $ filter (\x -> isDbDate x) db
    where
        isDbDate :: DatabaseItem -> Bool
        isDbDate (DbDate _) = True
        isDbDate _          = False

        getTime :: DatabaseItem -> UTCTime
        getTime (DbDate a) = a

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = map (\x -> getNum x) $ filter (\x -> isDbNum x) db
    where
        isDbNum :: DatabaseItem -> Bool
        isDbNum (DbNumber _) = True
        isDbNum _            = False

        getNum :: DatabaseItem -> Integer
        getNum (DbNumber a) = a

-- 3.
{- Note: In terms of the Ord type class, later dates are greater than earlier
   dates.  -}
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = maximum $ filterDbDate db

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 $ filterDbNumber db

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb db = 
    (fromIntegral $ sumDb db) / (fromIntegral $ length $ filterDbNumber db)
 
