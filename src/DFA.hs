{-# LANGUAGE TemplateHaskell #-}
module DFA where

import qualified Data.Set as S
import Data.List
import Control.Lens
import Control.Monad
import qualified Data.Map as M

import Text.Show.Functions

data Input a = Input a deriving (Show, Eq, Ord)

data State a = State a deriving (Show, Eq)

data Rule a b = Rule (State a) (Input b) (State a) deriving (Eq, Show)

matchRule :: (Eq a, Eq b) => Input b -> State a -> [Rule a b] -> Maybe (Rule a b)
matchRule i cs rs = find (\(Rule c l _) -> (cs == c) &&  (i == l)) rs

newtype Label a = Label a deriving (Eq, Show)
type Priority = Int
data GoalState a b = GoalState (State a) Priority (Label b) deriving (Eq, Show)

instance (Eq a, Eq b) => Ord (GoalState a b) where
  compare (GoalState _ p1 _) (GoalState _ p2 _) = compare p1 p2

goalStatep :: (Eq a, Eq b) => State a -> GoalState a b -> Bool
goalStatep s (GoalState gs _ _) = s == gs

-- a: State type b: Input type c: Label type
data DFA a b c = DFA {
    _fstState :: State a
    ,_currState :: State a
    ,_rules :: [(Rule a b)]
    ,_goalState :: [GoalState a c]
} deriving (Show)
$(makeLenses ''DFA)



mkDFA :: State a -> [Rule a b] -> [GoalState a c] -> DFA a b c
mkDFA s rs gs = DFA {
                    _fstState = s
                    ,_currState = s
                    ,_rules = rs
                    ,_goalState = gs}

updateDFA :: (Eq a, Eq b) => DFA a b c -> Input b -> Maybe (DFA a b c)
updateDFA dfa i = updateDFA' dfa r
  where
        r = matchRule i (dfa^.currState) (dfa^.rules)
        updateDFA' :: Eq a => DFA a b c -> Maybe (Rule a b) -> Maybe (DFA a b c)
        updateDFA' _ Nothing = Nothing
        updateDFA' dfa (Just (Rule _ _ ns)) = Just (dfa&currState.~ns)

runDFA :: (Eq a, Eq b) => DFA a b c -> [Input b] -> Maybe (DFA a b c)
runDFA df is = foldM updateDFA df is

accept :: (Eq a, Eq b, Eq c) => DFA a b c -> [b] -> Bool
accept df is = accept' res
  where
    res = runDFA df $ map (\x -> (Input x)) is
    accept' Nothing = False
    accept' (Just dfa) = any (\gs -> goalStatep (dfa^.currState) gs) (dfa^.goalState)

getLabel :: (Eq a) => DFA a b c -> Maybe (Label c)
getLabel dfa = getLabel' matchedGoal
  where
    matchedGoal = filter (\(GoalState gs _ _) -> gs == (dfa^.currState)) (dfa^.goalState)
    getLabel' [] = Nothing
    getLabel' (x:y:xs) = Nothing
    getLabel' ((GoalState _ _ l):[]) = Just l

showGraphViz :: (Show a, Show b, Show c) => DFA (S.Set a) b c -> IO ()
showGraphViz dfa = do {
  putStrLn "digraph dfa {";
  forM_ (dfa^.rules) (\(Rule (State fs) (Input i) (State ts)) -> do {
    putStrLn ("\"" ++ (show (S.toList fs)) ++ "\" -> \"" ++ (show (S.toList ts)) ++ "\" [label = \"" ++ (show i) ++ "\"];")
  });
  forM_ (dfa^.goalState) (\(GoalState (State fs) _ (Label l)) -> do {
    putStrLn ("\"" ++ (show (S.toList fs)) ++ "\" [peripherials = 2, label = " ++ (show l) ++ "];")
  });
  putStrLn "}";
}

showNorm :: (Show a, Show b, Show c) => DFA a b c -> IO ()
showNorm dfa = do {
  putStrLn "digraph dfa {";
  forM_ (dfa^.rules) (\(Rule (State fs) (Input i) (State ts)) -> do {
    putStrLn ((show fs) ++ " -> " ++ (show ts) ++ " [label = \"" ++ (show i) ++ "\"];")
  });
  forM_ (dfa^.goalState) (\(GoalState (State fs) _ (Label l)) -> do {
    putStrLn ((show fs) ++ " [peripherials = 2, label = " ++ (show l) ++ "];")
  });
  putStrLn "}";
}
