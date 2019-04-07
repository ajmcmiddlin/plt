{-# LANGUAGE TemplateHaskell #-}

module Error where

import           Control.Lens (makeClassyPrisms)
import           Data.Text    (Text)
import qualified Text.Parsec  as Parsec

import           HuttonsRazor (AsParseErrorType (_ParseErrorType), ParseErrorType)
import           Types        (AsTypeError (_TypeError), TypeError)

data Error =
  ETypeError TypeError
  | EParseError ParseErrorType
  deriving (Eq, Show)

makeClassyPrisms ''Error

instance AsTypeError Error where
  _TypeError =  _ETypeError

instance AsParseErrorType Error where
  _ParseErrorType = _EParseError
