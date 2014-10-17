{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EpsilonNFA where

import Data.Set
import qualified Data.List as L
import Control.Monad
import Control.Lens
import qualified DFA as D (State(State), Input(Input), Rule(Rule), DFA, mkDFA)
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

data EpsilonNFA a b = EpsilonNFA {
                _fstState :: D.State a
                ,_currState :: [D.State a]
                ,_rules :: [Rule a b]
                ,_goalState :: [D.State a]
                } deriving (Show)
$(makeLenses ''EpsilonNFA)

mkEpsilonNFA :: (Eq a) => D.State a -> [Rule a b] -> [D.State a] -> EpsilonNFA a b
mkEpsilonNFA s rs gs = EpsilonNFA {
                _fstState = s
                ,_currState = eclose [s] rs
                ,_rules = rs
                ,_goalState = gs
                }

updateEpsilonNFA :: (Eq a, Eq b) => EpsilonNFA a b -> D.Input b -> Maybe (EpsilonNFA a b)
updateEpsilonNFA enfa i = updateEpsilonNFA' enfa nxtStates
  where
    rs = concat $ L.map (\s -> matchRule i s (enfa^.rules))
                        (enfa^.currState)
    nxtStates = eclose (L.map (\(Rule _ _ ns) -> ns) rs) (enfa^.rules)
    updateEpsilonNFA' :: (Eq a) => EpsilonNFA a b -> [D.State a] -> Maybe (EpsilonNFA a b)
    updateEpsilonNFA' _ [] = Nothing
    updateEpsilonNFA' nfa ns = Just (nfa&currState.~ns)

runEpsilonNFA :: (Eq a, Eq b) => EpsilonNFA a b -> [D.Input b] -> Maybe (EpsilonNFA a b)
runEpsilonNFA enfa is = foldM updateEpsilonNFA enfa is

accept :: (Eq a, Eq b) => EpsilonNFA a b -> [b] -> Bool
accept enfa is = accept' res
  where
    res = runEpsilonNFA enfa $ L.map (\x -> (D.Input x)) is
    accept' Nothing = False
    accept' (Just f) = L.any (\s -> elem s (f^.goalState)) (f^.currState)

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

genDFA :: forall a b. (Ord a) => EpsilonNFA a b -> D.DFA (Set a) b
genDFA enfa = D.mkDFA fst dfaRules dfaGoal
  where
    fstClose = eclose [(enfa^.fstState)] (enfa^.rules)
    fst :: D.State (Set a)
    fst = D.State $ fromList $ L.map (\(D.State x) -> x) fstClose
    enfaRules = enfa^.rules
    fstDFARules = genDFARule' fst enfaRules
    fstTmpStates = L.filter (\x -> x /= fst) $ L.map (\(D.Rule _ _ x) -> x) fstDFARules
    dfaRules = (genDFARule'' enfaRules fstDFARules [fst] fstTmpStates)
    dfaGoal = L.nub $ L.filter (\(D.State t) -> any (\(D.State x) -> member x t) (enfa^.goalState)) $ [fst] ++ (L.map (\(D.Rule _ _ g) -> g) dfaRules)
