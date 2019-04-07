{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module HuttonsRazor where

import           Control.Applicative     ((<|>))
import           Control.Monad.Error.Lens  (throwing)
import           Control.Lens            (Prism', makeClassyPrisms, makeWrapped)
import           Control.Monad.Except    (MonadError)
import           Data.Text               (Text, pack)
import qualified Text.Parsec             as Parsec
import           Text.Parser.Combinators (many)
import           Text.Parser.Token       (TokenParsing, integer, parens, symbol,
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

newtype Precedence =
  Prec Int
  deriving (Eq, Show, Ord)

precIfThenElse, precAdd, precOr :: Precedence
precIfThenElse = Prec 1
precAdd = Prec 3
precOr = Prec 3

incPrec ::
  Precedence
  -> Precedence
incPrec (Prec n) =
  Prec $ n + 1

pretty ::
  Razor
  -> Text
pretty =
  prettyPrec (Prec 0)

prettyPrec ::
  Precedence
  -> Razor
  -> Text
prettyPrec p = \case
  LitI n -> pack $ show n
  LitB True -> "true"
  LitB False -> "false"
  IfThenElse rb ra1 ra2 ->
    let
      pIte =
        prettyPrec precIfThenElse
      ite =
        "if " <> pIte rb <> " then " <> pIte ra1 <> " else " <> pIte ra2
    in
      prettyParen p precIfThenElse ite
  Add r1 r2 -> prettyParen p precAdd $
    prettyPrec (incPrec precAdd) r1 <> " + " <> prettyPrec (incPrec precAdd) r2
  Or b1 b2 -> prettyParen p precOr $
    prettyPrec (incPrec precOr) b1 <> " || " <> prettyPrec (incPrec precOr) b2

prettyParen ::
  Precedence
  -> Precedence
  -> Text
  -> Text
prettyParen pOuter pInner t =
  if pOuter > pInner then
    "(" <> t <> ")"
  else
    t

newtype ParseErrorType =
  ParseError Parsec.ParseError
  deriving (Eq, Show)

makeWrapped ''ParseErrorType
makeClassyPrisms ''ParseErrorType

parseText ::
  ( MonadError e m
  , AsParseErrorType e
  )
  => Text
  -> m Razor
parseText =
  either (throwing _ParseError) pure . Parsec.parse parseRazor ""

parseRazor ::
  TokenParsing m
  => m Razor
parseRazor =
  let
    baseParser =
      foldr1 (<|>) [parseLitI, parseLitB, parseIfThenElse]
  in
    foldr ($) <$> (parens parseRazor <|> baseParser) <*> many (parseAdd <|> parseOr)

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

parseLitB ::
  TokenParsing m
  => m Razor
parseLitB =
  LitB  <$> (True <$ symbol "true" <|> False <$ symbol "false")

parseOr ::
  TokenParsing m
  => m (Razor -> Razor)
parseOr =
  flip Or <$> (symbol "||" *> parseRazor)

parseIfThenElse ::
  TokenParsing m
  => m Razor
parseIfThenElse =
  let
    f s = symbol s *> parseRazor
  in
    IfThenElse <$> f "if" <*> f "then" <*> f "else"
