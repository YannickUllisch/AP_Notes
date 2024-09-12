
-- Lecture 1 Code
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

fib :: Int -> Int
fib n =
  if n < 2 then 1 else fib (n - 1) + fib (n - 2)


data Q = Sure | No
  deriving (Show)

val :: Q -> Int
val q = case q of 
      Sure -> 1
      No -> 0

data List = Nil | Cons Int List 
  deriving(Show)

listLength :: List -> Int
listLength Nil = 0
listLength (Cons _x xs) = 1 + listLength xs


-- Phantom Types
data Measure unit = Measure Double
data Kilogram
data MeterPerSecond
data Joule

mass :: Measure Kilogram 
mass = Measure 10

velo :: Measure MeterPerSecond
velo = Measure 20

energy :: Measure Kilogram -> Measure MeterPerSecond -> Measure Joule
energy (Measure m) (Measure v) = Measure(0.5 * v * ( v ** 2) )

-- Physics stuff
-- data Kilogram = Kilogram Double
-- data MeterPerSecond = MeterPerSecond Double
-- data Joule = Joule Double
--   deriving(Show)

-- kinetic :: Kilogram -> MeterPerSecond -> Joule
-- kinetic (Kilogram m) (MeterPerSecond v) = Joule(0.5 * m * (v ** 2))

-- mass :: Kilogram
-- mass = Kilogram 10

-- velo :: MeterPerSecond
-- velo = MeterPerSecond 20
