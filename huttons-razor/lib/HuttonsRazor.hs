{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module HuttonsRazor where

import           Control.Applicative     ((<|>))
import           Data.Text               (Text)
import           Text.Parsec             (ParseError, parse)
import           Text.Parser.Combinators (many)
import           Text.Parser.Token       (TokenParsing, integer, symbol,
                                          symbolic)

-- | Solution for https://www.codewars.com/kata/huttons-razor, followed by some
-- spicy additions recommended by Dave.

data Razor where
  Razor :: RazorT a -> Razor
  -- LitI Integer
  -- | LitB Bool
  -- | IfThenElse Razor Razor Razor
  -- | Add Razor Razor
  -- deriving (Show)

data RazorT a where
  LitIT :: Integer -> RazorT Integer
  LitBT :: Bool -> RazorT Bool
  IfThenElseT :: RazorT Bool -> RazorT a -> RazorT a -> RazorT a
  AddT :: RazorT Integer -> RazorT Integer -> RazorT Integer

deriving instance Show (RazorT a)

ex1 :: RazorT Integer
ex1 = AddT (LitIT 1) (LitIT 2)

ex2 :: RazorT Integer
ex2 = AddT (LitIT 1) (AddT (LitIT 2) (AddT (LitIT 3) (LitIT 4)))

interpret ::
  RazorT a
  -> a
interpret = \case
  LitIT n -> n
  LitBT b -> b
  IfThenElseT (LitBT b) raTrue raFalse -> if b then interpret raTrue else interpret raFalse
  AddT r1 r2 -> interpret r1 + interpret r2

pretty ::
  RazorT a
  -> String
pretty = \case
  LitIT n -> show n
  LitBT b -> show b
  IfThenElseT rb ra1 ra2 -> "if " <> pretty rb <> " then " <> pretty ra1 <> " else " <> pretty ra2
  AddT r1 r2 -> "(" <> pretty r1 <> "+" <> pretty r2 <> ")"

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
  parseLitIOrAdd <|> parseLitB <|> parseIfThenElse

parseLitI ::
  TokenParsing m
  => m Razor
parseLitI =
  Razor . LitIT <$> integer

parseAdd ::
  forall m a.
  TokenParsing m
  => m (RazorT a -> Razor)
parseAdd =
  let
    b :: m Razor
    b = symbolic '+' *> parseRazor

    f :: Razor -> RazorT Integer -> RazorT Integer
    f (Razor (rt :: RazorT Integer)) = flip AddT rt
  in
    _ $ (\case (Razor rt) -> flip AddT rt) <$> b

parseLitIOrAdd ::
  TokenParsing m
  => m Razor
parseLitIOrAdd =
  foldr ($) <$> parseLitI <*> many parseAdd

parseLitB ::
  TokenParsing m
  => m Razor
parseLitB =
  LitB . (== "true") <$> (symbol "true" <|> symbol "false")

parseIfThenElse ::
  TokenParsing m
  => m Razor
parseIfThenElse =
  let
    f s = symbol s *> parseRazor
  in
    IfThenElse <$> f "if" <*> f "then" <*> f "else"

