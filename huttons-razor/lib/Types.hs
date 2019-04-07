{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens             (Prism', prism)
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
  deriving (Eq, Show)

class AsTypeError r_auEC where
      _TypeError :: Prism' r_auEC TypeError

      _MismatchedTypes :: Prism' r_auEC (Text, Type, Type)
      _MismatchedTypes = _TypeError . _MismatchedTypes

instance AsTypeError TypeError where
  _TypeError = id
  _MismatchedTypes =
    prism
      (\(x1_auED, x2_auEE, x3_auEF) ->
        MismatchedTypes x1_auED x2_auEE x3_auEF)
      (\case
        MismatchedTypes y1_auEH y2_auEI y3_auEJ ->
          Right (y1_auEH, y2_auEI, y3_auEJ)
      )

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
