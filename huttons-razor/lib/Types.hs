{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens             (makeClassyPrisms)
import           Control.Monad            (void)
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
      bool (terror $ MismatchedTypes op ty1 ty2) (pure ty1) $ ty1 == ty2

check ::
  ( AsTypeError e
  , MonadError e m
  )
  => Type
  -> Razor
  -> m ()
check tyExpected = \case
  (LitI _) -> checkLit "integer" TyInteger
  (LitB _) -> checkLit "boolean" TyBool
  (Add r1 r2) ->
    void . sequence $ [
      check TyInteger r1
    , check TyInteger r2
    , check' UnexpectedType "+" tyExpected TyInteger
    ]
  (Or r1 r2) ->
    void . sequence $ [
      check TyBool r1
    , check TyBool r2
    , check' UnexpectedType "||" tyExpected TyBool
    ]
  (IfThenElse b t f) -> do
    tyT <- infer t
    tyF <- infer f
    void . sequence $ [
        check TyBool b
      , check' MismatchedTypes "if" tyT tyF
      , check' UnexpectedType "if" tyExpected tyT
      ]
  where
    check' f desc tyExpected' ty =
      bool
        (terror $ f desc tyExpected' ty)
        (pure ())
        (tyExpected == ty)
    checkLit name =
      check' UnexpectedType (name <> " literal") tyExpected

terror ::
  ( AsTypeError e
  , MonadError e m
  )
  => TypeError
  -> m a
terror =
  throwing _TypeError