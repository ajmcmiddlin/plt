{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens             (makeClassyPrisms)
import           Control.Monad.Error.Lens (throwing)
import           Control.Monad.Except     (MonadError)
import           Data.Bool                (bool)
import           Data.Text                (Text)

import           HuttonsRazor

data Type =
  TyInteger
  | TyBool
  deriving (Eq, Show)

data TypeError =
  MismatchedTypes Text Type Type
  | UnexpectedType Text Type Type
  deriving (Eq, Show)

makeClassyPrisms ''TypeError

infer ::
  ( AsTypeError e
  , MonadError e m)
  => Razor
  -> m Type
infer = \case
  (LitI _) -> pure TyInteger
  (LitB _) -> pure TyBool
  (Add r1 r2) -> binOp "+" r1 r2
  (Or r1 r2) -> binOp "||" r1 r2
  (IfThenElse _ t f) -> binOp "if" t f
  where
    binOp op r1 r2 = do
      ty1 <- infer r1
      ty2 <- infer r2
      bool (mismatch op ty1 ty2) (pure ty1) $ ty1 == ty2
    mismatch op ty1 ty2 =
      throwing _TypeError $ MismatchedTypes op ty1 ty2
