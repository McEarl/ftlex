-- |
-- Creator: Marcel Schütz (2024)
--
-- Helpers.

module Flex.Helpers (
  disjunction
) where

disjunction :: [a -> Bool] -> a -> Bool
disjunction [] x = False
disjunction (p : ps) x = p x || disjunction ps x
