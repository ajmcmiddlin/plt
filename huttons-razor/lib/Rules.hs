{-# LANGUAGE LambdaCase #-}

module Rules where

import           Data.Foldable (asum)
import           HuttonsRazor  (Razor (..))

-- | Checks natural deduction rules.
type RuleSet a b = a -> Maybe b

type Rule a b = RuleSet a b -> a -> Maybe b

mkRuleSet ::
  [Rule a b]
  -> RuleSet a b
mkRuleSet rules =
  let
    rs a =
      asum $ fmap (\r -> r rs a) rules
  in
    rs

stepAddLitI ::
  Rule Razor Razor
stepAddLitI _ = \case
  Add (LitI n1) (LitI n2) -> Just . LitI $ n1 + n2
  _ -> Nothing

stepAddL ::
  Rule Razor Razor
stepAddL rs = \case
  Add l r -> Add <$> rs l <*> pure r
  _ -> Nothing

stepAddR ::
  Rule Razor Razor
stepAddR rs = \case
  Add l r -> Add l <$> rs r
  _ -> Nothing

stepIfThenElseReduce ::
  Rule Razor Razor
stepIfThenElseReduce rs = \case
  IfThenElse c t f -> IfThenElse <$> rs c <*> pure t <*> pure f
  _ -> Nothing

stepIfThenElseLit ::
  Rule Razor Razor
stepIfThenElseLit _ = \case
  IfThenElse (LitB True) t _ -> Just t
  IfThenElse (LitB False) _ f -> Just f
  _ -> Nothing

ruleSet ::
  RuleSet Razor Razor
ruleSet =
  mkRuleSet [stepAddLitI, stepAddR, stepAddL, stepIfThenElseReduce, stepIfThenElseLit]

-- | Iterate on a RuleSet to reduce something (e.g. a term)
iterR ::
  Rule a a
iterR rs a =
  maybe (Just a) (iterR rs) $ rs a
