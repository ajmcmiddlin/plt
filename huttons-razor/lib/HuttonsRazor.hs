{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HuttonsRazor where

import           Control.Applicative     ((<|>))
import           Data.Text               (Text, pack)
import           Text.Parsec             (ParseError, parse)
import           Text.Parser.Combinators (many)
import           Text.Parser.Token       (TokenParsing, integer, symbol,
                                          symbolic)

-- | Solution for https://www.codewars.com/kata/huttons-razor, followed by some
-- spicy additions recommended by Dave.

data Razor =
  LitI Integer
  | LitB Bool
  | IfThenElse Razor Razor Razor
  | Add Razor Razor
  | Or Razor Razor
  deriving (Eq, Show)

data Type =
  TyInteger
  | TyBool
  deriving (Eq, Show)

data RazorT =
  RazorT Razor Type
  deriving (Eq, Show)

ex1 :: Razor
ex1 = Add (LitI 1) (LitI 2)

ex2 :: Razor
ex2 = Add (LitI 1) (Add (LitI 2) (Add (LitI 3) (LitI 4)))

pretty ::
  Razor
  -> Text
pretty = \case
  LitI n -> pack $ show n
  LitB True -> "true"
  LitB False -> "false"
  IfThenElse rb ra1 ra2 -> "if " <> pretty rb <> " then " <> pretty ra1 <> " else " <> pretty ra2
  Add r1 r2 -> "(" <> pretty r1 <> " + " <> pretty r2 <> ")"
  Or b1 b2 -> "(" <> pretty b1 <> " || " <> pretty b2 <> ")"

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
  parseLitIOrAdd <|> parseIfThenElse <|> parseLitBOrOr

parseLitI ::
  TokenParsing m
  => m Razor
parseLitI =
  LitI <$> integer

parseAdd ::
  TokenParsing m
  => m (Razor -> Razor)
parseAdd =
  flip Add <$> (symbolic '+' *> parseRazor)

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

parseOr ::
  TokenParsing m
  => m (Razor -> Razor)
parseOr =
  flip Or <$> (symbol "||" *> parseRazor)

parseLitBOrOr ::
  TokenParsing m
  => m Razor
parseLitBOrOr =
  foldr ($) <$> parseLitB <*> many parseOr

parseIfThenElse ::
  TokenParsing m
  => m Razor
parseIfThenElse =
  let
    f s = symbol s *> parseRazor
  in
    IfThenElse <$> f "if" <*> f "then" <*> f "else"

