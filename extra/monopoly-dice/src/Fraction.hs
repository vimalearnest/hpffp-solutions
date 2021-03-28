{- I have made a simple Fraction library for the problem. All I
   have done here is implement a Fraction type that wraps an
   integer numerator and denominator, and then created instances of Eq,
   Ord, Show, and Num for it. I have also implemented some convenience
   functions.  -}

module Fraction where

data Fraction = Fraction
    { fractionNumerator   :: Integer
    , fractionDenominator :: Integer }

instance Show Fraction where
    show (Fraction n d) = 
        if   (signum n == (-1)) || (signum d == (-1))
        then "-" ++ (show . abs $ n) ++ "/" ++ (show . abs $ d)
        else (show n) ++ "/" ++ (show d)

instance Eq Fraction where
    (==) a b = fractionToDouble a == fractionToDouble b

instance Ord Fraction where
    (<)  a b = fractionToDouble a <  fractionToDouble b

    (<=) a b = fractionToDouble a <= fractionToDouble b

    (>)  a b = fractionToDouble a >  fractionToDouble b

    (>=) a b = fractionToDouble a >= fractionToDouble b
    
    max a b = 
        if   fractionToDouble b > fractionToDouble a
        then b
        else a
    
    min a b = 
        if   fractionToDouble b < fractionToDouble a
        then b
        else a

instance Num Fraction where
    (+) (Fraction n1 d1) (Fraction n2 d2) = 
        let m = lcm d1 d2
        in  simplifyFraction $ Fraction ((n1 * m `div` d1) + (n2 * m `div` d2)) m

    (*) (Fraction n1 d1) (Fraction n2 d2) = 
        simplifyFraction $ Fraction (n1 * n2) (d1 * d2)

    (-) (Fraction n1 d1) (Fraction n2 d2) =
        let m = lcm d1 d2
        in  simplifyFraction $ Fraction ((n1 * m `div` d1) - (n2 * m `div` d2)) m

    negate (Fraction n d) = Fraction (- n) d

    abs (Fraction n d)    = Fraction (abs n) (abs d)

    signum (Fraction n d) = 
        if   (signum n == (-1)) || (signum d == (-1))
        then (- 1)
        else 1

    fromInteger a = Fraction a 1

fractionToDouble :: Fraction -> Double
fractionToDouble (Fraction n d) = (fromInteger n) / (fromInteger d)

simplifyFraction :: Fraction -> Fraction
simplifyFraction a@(Fraction n d) = 
    let   b = gcd n d in
    if    b == 1
    then  a
    else  simplifyFraction $ Fraction (n `div` b) (d `div` b)

fracDiv :: Fraction -> Fraction -> Fraction
fracDiv (Fraction n1 d1) (Fraction n2 d2) = 
    simplifyFraction $ Fraction (n1 * d2) (d1 * n2)

reciprocal :: Fraction -> Fraction
reciprocal a = fracDiv (Fraction 1 1) a

