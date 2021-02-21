module Chap7ArtfulDodgy where

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy 2

{-
    oneIsOne = \y -> 1 + y * 10

    fDodgy = (flip dodgy)
    fDodgy = \x y -> y + x * 10

    Hence,
     oneIsTwo = fDodgy 2 = fDodgy = \y -> y + 20
    Or,
    oneIsTwo = \y -> y + 20

    Prelude> dodgy 1 1
    11
    Prelude> dodgy 2 2
    22
    Prelude> dodgy 1 2
    21
    Prelude> dodgy 2 1
    12

    Prelude> oneIsOne 1
    11
    Prelude> oneIsOne 2
    21

    Prelude> oneIsTwo 1
    21
    Prelude> oneIsTwo 2
    22

    Prelude> oneIsOne 3
    31
    Prelude> oneIsTwo 3
    23
-}