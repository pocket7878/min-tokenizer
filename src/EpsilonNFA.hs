{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EpsilonNFA where

import Data.Set
import qualified Data.List as L
import Control.Monad
import Control.Lens
import qualified DFA as D (State(State), Input(Input), Rule(Rule), Label(Label), GoalState(GoalState), Priority, DFA, mkDFA)
import qualified Debug.Trace as De

data Epsilon = Epsilon deriving (Show, Eq)

data Rule a b = Rule (D.State a) (Either (D.Input b) Epsilon) (D.State a) deriving (Eq, Show)

matchRule :: (Eq a, Eq b) => D.Input b -> D.State a -> [Rule a b] -> [Rule a b]
matchRule i cs rs =
    L.filter (\(Rule c y _) -> case y of
                                 (Left l) -> (cs == c) && (i == l)
                                 (Right _) -> False) rs

--Epsilonルール１ステップで移動可能な状態の集合を得る
eclose' :: (Eq a) => D.State a -> [Rule a b] -> [D.State a]
eclose' s rs = enext
  where
    erule = L.filter (\(Rule x y _) -> case y of
                                         (Right _) -> (x == s)
                                         (Left _) -> False) rs
    enext = L.map (\(Rule _ _ ns) -> ns) erule

--Epsilonルールで移動可能な状態の集合を得る
eclose'' :: (Eq a) => [D.State a] -> [Rule a b] -> [D.State a] -> [D.State a]
eclose'' s rs acc
  | epNexts == [] = acc
  | otherwise = eclose'' s rs (acc ++ epNexts)
  where
    epNexts = L.filter (\es -> not (elem es acc)) $
                concat $ L.map (\a -> eclose' a rs) acc

eclose :: (Eq a) => [D.State a] -> [Rule a b] -> [D.State a]
eclose s rs = eclose'' s rs s

data EpsilonNFA a b c = EpsilonNFA {
                _fstState :: D.State a
                ,_currState :: [D.State a]
                ,_rules :: [Rule a b]
                ,_goalState :: [D.GoalState a c]
                } deriving (Show)
$(makeLenses ''EpsilonNFA)

mkEpsilonNFA :: (Eq a) => D.State a -> [Rule a b] -> [D.GoalState a c] -> EpsilonNFA a b c
mkEpsilonNFA s rs gs = EpsilonNFA {
                _fstState = s
                ,_currState = eclose [s] rs
                ,_rules = rs
                ,_goalState = gs
                }

updateEpsilonNFA :: (Eq a, Eq b) => EpsilonNFA a b c -> D.Input b -> Maybe (EpsilonNFA a b c)
updateEpsilonNFA enfa i = updateEpsilonNFA' enfa nxtStates
  where
    rs = concat $ L.map (\s -> matchRule i s (enfa^.rules))
                        (enfa^.currState)
    nxtStates = eclose (L.map (\(Rule _ _ ns) -> ns) rs) (enfa^.rules)
    updateEpsilonNFA' :: (Eq a) => EpsilonNFA a b c -> [D.State a] -> Maybe (EpsilonNFA a b c)
    updateEpsilonNFA' _ [] = Nothing
    updateEpsilonNFA' nfa ns = Just (nfa&currState.~ns)

runEpsilonNFA :: (Eq a, Eq b) => EpsilonNFA a b c -> [D.Input b] -> Maybe (EpsilonNFA a b c)
runEpsilonNFA enfa is = foldM updateEpsilonNFA enfa is

accept :: (Eq a, Eq b) => EpsilonNFA a b c -> [b] -> Bool
accept enfa is = accept' res
  where
    res = runEpsilonNFA enfa $ L.map (\x -> (D.Input x)) is
    accept' Nothing = False
    accept' (Just f) = L.any (\s -> (L.any (\(D.GoalState gs _ _) -> gs == s) (f^.goalState))) (f^.currState)

{-
- Convert Epsilon-NFA -> DFA
-}

type ENFARule a b = Rule a b
type DFARule a b = D.Rule (Set a) b
genDFARule' :: forall a b. (Ord a) => (D.State (Set a)) -> [ENFARule a b] -> [DFARule a b]
genDFARule' (D.State s) rs = L.map nxt matchedRules
  where
    matchedRules :: [ENFARule a b]
    matchedRules = L.filter (\(Rule (D.State x) y _) -> case y of
                                                          (Right _) -> False
                                                          (Left _) -> (member x s)) rs
    nxt ::  ENFARule a b -> DFARule a b
    nxt (Rule (D.State f) (Left i) t) = D.Rule (D.State s) i (D.State (fromList (L.map (\(D.State x) -> x) (eclose [t] rs))))
    nxt (Rule _ (Right Epsilon) t) = error "Can't convert epsilon rule to dfa-rule. (Illigal state)"

genDFARule'' :: forall a b. (Ord a) => [ENFARule a b] -> [DFARule a b] -> [D.State (Set a)] -> [D.State (Set a)] -> [DFARule a b]
genDFARule'' rs acc visitedStates tmpStates
  | L.null newTmpStates = acc ++ generatedRules
  | otherwise = genDFARule'' rs (generatedRules ++ acc) (visitedStates ++ newTmpStates) newTmpStates
  where
    generatedRules :: [DFARule a b]
    generatedRules = concat $ L.map (\x -> genDFARule' x rs) tmpStates
    newTmpStates = L.filter (\x -> not (elem x visitedStates)) $ L.map (\(D.Rule _ _ x) -> x) generatedRules

genDFAGoalFromDFAState :: (Eq a, Eq b, Ord a) => D.State (Set a) -> [D.GoalState a b] -> D.GoalState (Set a) b
genDFAGoalFromDFAState (D.State as) gs = D.GoalState (D.State as) goalPriority goalLabel
  where
    goalStates = L.filter (\(D.GoalState (D.State g) _ _) -> member g as) gs
    (D.GoalState _ goalPriority goalLabel) = maximum goalStates

genDFA :: forall a b c. (Ord a, Eq c) => EpsilonNFA a b c -> D.DFA (Set a) b c
genDFA enfa = D.mkDFA fst dfaRules dfaGoals
  where
    fstClose = eclose [(enfa^.fstState)] (enfa^.rules)
    fst :: D.State (Set a)
    fst = D.State $ fromList $ L.map (\(D.State x) -> x) fstClose
    enfaRules = enfa^.rules
    fstDFARules = genDFARule' fst enfaRules
    fstTmpStates = L.filter (\x -> x /= fst) $ L.map (\(D.Rule _ _ x) -> x) fstDFARules
    dfaRules = (genDFARule'' enfaRules fstDFARules [fst] fstTmpStates)
    tmpDfaGoals = L.nub $ L.filter (\(D.State t) -> any (\(D.GoalState (D.State x) _ _) -> member x t) (enfa^.goalState)) $ [fst] ++ (L.map (\(D.Rule _ _ g) -> g) dfaRules)
    dfaGoals = L.map (\t -> genDFAGoalFromDFAState t (enfa^.goalState)) tmpDfaGoals
