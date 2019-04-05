{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator where

import           Control.Monad.Except (MonadError (catchError, throwError))
import           Data.Bool            (bool)
import           Data.Text            (Text, pack)
import           Text.Parsec          (ParseError)

import           HuttonsRazor

data EvalError =
  TypeError Text
  | NormalForm
  | EvalParseError ParseError
  deriving (Show, Eq)

eval ::
  (MonadError EvalError m
  , Show (m Razor))
  => Razor
  -> m Razor
eval r =
  go (step r) r
  where
    go mr rPrev =
      catchError ((\rNow -> go (step rNow) rNow) =<< mr) $ handle rPrev
    handle rPrev = \case
      NormalForm -> pure rPrev
      e -> throwError e

step ::
  MonadError EvalError m
  => Razor
  -> m Razor
step = \case
  LitI _ -> throwError NormalForm
  LitB _ -> throwError NormalForm

  Add (LitI a) (LitI b) -> pure . LitI $ a + b
  Add (LitB _) _ -> badFirstType "+" TyBool TyInteger
  Add _ (LitB _) -> badSecondType "+" TyBool TyInteger
  Add rl@(LitI _) rr -> Add rl <$> step rr
  Add rl rr -> Add <$> step rl <*> pure rr

  Or (LitB b1) (LitB b2) -> pure . LitB $ b1 || b2
  Or (LitI _) _ -> badFirstType "||" TyInteger TyBool
  Or _ (LitI _) -> badSecondType "||" TyInteger TyBool
  Or rl@(LitB _) rr -> Or rl <$> step rr
  Or rl rr -> Or <$> step rl <*> pure rr

  IfThenElse (LitB b) rt rf -> pure $ bool rf rt b
  IfThenElse (LitI _) _ _ -> badFirstType "if/then/else" TyInteger TyBool
  IfThenElse b rt rf -> IfThenElse <$> step b <*> pure rt <*> pure rf

badFirstType, badSecondType ::
  MonadError EvalError m
  => Text
  -> Type
  -> Type
  -> m a

badFirstType op tyActual tyExpected =
  throwError . TypeError $
    "First argument to `" <> op <> "` has type " <> ts tyActual <> ". Expected "
    <> ts tyExpected <> "."

badSecondType op tyActual tyExpected =
  throwError . TypeError $
    "Second argument to `" <> op <> "` has type " <> ts tyActual <> ". Expected "
    <> ts tyExpected <> "."

ts ::
  Show a
  => a
  -> Text
ts =
  pack . show
