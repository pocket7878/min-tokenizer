{-# LANGUAGE TemplateHaskell #-}
module NFA where

import Data.List
import Control.Monad
import Control.Lens
import qualified DFA as D (Input(Input), State(State), Rule(Rule))
import Text.Show.Functions

matchRule :: (Eq a, Eq b) => D.Input b -> D.State a -> [D.Rule a b] -> [D.Rule a b]
matchRule i cs rs =
    filter (\(D.Rule c l _) -> (cs == c) && (l == i)) rs

data NFA a b = NFA {
         _fstState :: D.State a
         ,_currState :: [D.State a]
         ,_rules :: [D.Rule a b]
         ,_goalState :: [D.State a]
} deriving (Show)
$(makeLenses ''NFA)


mkNFA :: D.State a -> [D.Rule a b] -> [D.State a] -> NFA a b
mkNFA s rs gs = NFA {
          _fstState = s
          ,_currState = [s]
          ,_rules = rs
          ,_goalState = gs}

updateNFA :: (Eq a, Eq b) => NFA a b -> D.Input b -> Maybe (NFA a b)
updateNFA nfa i = updateNFA' nfa nxtStates
  where
    rs = concat $ map (\s -> matchRule i s (nfa^.rules))
                        (nfa^.currState)
    nxtStates = map (\(D.Rule _ _ ns) -> ns) rs
    updateNFA' :: (Eq a) => NFA a b-> [D.State a] -> Maybe (NFA a b)
    updateNFA' _ [] = Nothing
    updateNFA' nfa ns = Just (nfa&currState.~ns)

runNFA :: (Eq a, Eq b) => NFA a b -> [D.Input b] -> Maybe (NFA a b)
runNFA nf is = foldM updateNFA nf is

accept :: (Eq a, Eq b) => NFA a b -> [b] -> Bool
accept nfa is = accept' res
  where
    res = runNFA nfa $ map (\x -> (D.Input x)) is
    accept' Nothing = False
    accept' (Just dfa) = any (\s -> elem s (dfa^.goalState))
                                (dfa^.currState)
