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
mkRuleSet rs =
  let
    ruleSet a =
      asum $ fmap (\r -> r ruleSet a) rs
  in
    ruleSet

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

-- | Iterate on a RuleSet to reduce something (e.g. a term)
iterR ::
  Rule a a
iterR rs a =
  maybe (Just a) (iterR rs) $ rs a
