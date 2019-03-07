{-# LANGUAGE LambdaCase #-}

module HuttonsRazor where

import           Data.Text               (Text)
import           Text.Parsec             (ParseError, parse)
import           Text.Parser.Combinators (many)
import           Text.Parser.Token       (TokenParsing, integer, symbolic)

-- | Solution for https://www.codewars.com/kata/huttons-razor, followed by some
-- spicy additions recommended by Dave.

data Razor =
  Lit Int
  | Add Razor Razor
  deriving (Show)

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

parseText ::
  Text
  -> Either ParseError Razor
parseText =
  parse parseRazor ""

-- | The grammar for Hutton's Razor is:
--
-- V = {Addition, Razor}
-- T = {n, +}
-- S = {Razor}
-- P = { Addition -> Razor + Razor
--     , Razor -> n | Addition
--     }
--
-- However, this is left recursive. To avoid this we change our production rules.
--
-- P' = { Razor -> n*Addition
--      , Addition -> \+n
--      }
parseRazor ::
  TokenParsing m
  => m Razor
parseRazor =
  let
    parseLit = Lit . fromInteger <$> integer
    parseAdd = flip Add <$> (symbolic '+' *> parseRazor)
  in
    foldr ($) <$> parseLit <*> many parseAdd
