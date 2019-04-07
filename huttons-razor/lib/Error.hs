{-# LANGUAGE LambdaCase #-}

module Error where

import           Control.Lens (Prism', prism)

import           HuttonsRazor (AsParseErrorType (_ParseErrorType),
                               ParseErrorType)
import           Types        (AsTypeError (_TypeError), TypeError)

data Error =
  ETypeError TypeError
  | EParseError ParseErrorType
  deriving (Eq, Show)

class AsError e where
  _Error :: Prism' e Error

  _ETypeError :: Prism' e TypeError
  _ETypeError = _Error . _ETypeError

  _EParseError :: Prism' e ParseErrorType
  _EParseError = _Error . _EParseError

instance AsError Error where
  _Error =
    id
  _ETypeError =
    prism ETypeError
    (\case
      ETypeError te -> Right te
      e -> Left e)
  _EParseError =
    prism EParseError
    (\case
        EParseError y1_av6z -> Right y1_av6z
        e -> Left e)

instance AsTypeError Error where
  _TypeError =  _ETypeError

instance AsParseErrorType Error where
  _ParseErrorType = _EParseError
