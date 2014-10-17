{-# LANGUAGE TemplateHaskell #-}
module DFA where

import Data.List
import Control.Lens
import Control.Monad

import Text.Show.Functions

data Input a = Input a deriving (Show, Eq)

data State a = State a deriving (Show, Eq)

data Rule a b = Rule (State a) (Input b) (State a) deriving (Eq, Show)

matchRule :: (Eq a, Eq b) => Input b -> State a -> [Rule a b] -> Maybe (Rule a b)
matchRule i cs rs = find (\(Rule c l _) -> (cs == c) &&  (i == l)) rs

-- a: State type b: Input type
data DFA a b = DFA {
    _fstState :: State a
    ,_currState :: State a
    ,_rules :: [(Rule a b)]
    ,_goalState :: [State a]
} deriving (Show)
$(makeLenses ''DFA)

mkDFA :: State a -> [Rule a b] -> [State a] -> DFA a b
mkDFA s rs gs = DFA {
                    _fstState = s
                    ,_currState = s
                    ,_rules = rs
                    ,_goalState = gs}

updateDFA :: (Eq a, Eq b) => DFA a b -> Input b -> Maybe (DFA a b)
updateDFA dfa i = updateDFA' dfa r
  where
        r = matchRule i (dfa^.currState) (dfa^.rules)
        updateDFA' :: Eq a => DFA a b -> Maybe (Rule a b) -> Maybe (DFA a b)
        updateDFA' _ Nothing = Nothing
        updateDFA' dfa (Just (Rule _ _ ns)) = Just (dfa&currState.~ns)

runDFA :: (Eq a, Eq b) => DFA a b -> [Input b] -> Maybe (DFA a b)
runDFA df is = foldM updateDFA df is

accept :: (Eq a, Eq b) => DFA a b -> [b] -> Bool
accept df is = accept' res
  where
    res = runDFA df $ map (\x -> (Input x)) is
    accept' Nothing = False
    accept' (Just dfa) = elem (dfa^.currState) (dfa^.goalState)
