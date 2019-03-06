{-# LANGUAGE LambdaCase #-}

module HuttonsRazor where

-- | Solution for https://www.codewars.com/kata/huttons-razor, followed by some
-- spicy additions recommended by Dave.

data Razor =
  Lit Int
  | Add Razor Razor

ex1 :: Razor
ex1 = Add (Lit 1) (Lit 2)

ex2 :: Razor
ex2 = Add (Lit 1) (Add (Lit 2) (Add (Lit 3) (Lit 4)))

interpret ::
  Razor
  -> Int
interpret = \case
  Lit n -> n
  Add r1 r2 -> interpret r1 + interpret r2

pretty ::
  Razor
  -> String
pretty = \case
  Lit n -> show n
  Add r1 r2 -> "(" <> pretty r1 <> "+" <> pretty r2 <> ")"