{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EpsilonNFA where

import Data.Set
import qualified Data.List as L
import Control.Monad
import Control.Lens
import qualified DFA as D (State(State), Input(Input), Rule(Rule), Label(Label), GoalState(GoalState), Priority, DFA, mkDFA)
import qualified Debug.Trace as De
import qualified Data.Map as M

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

incrementStateId :: D.State Int -> Int -> D.State Int
incrementStateId (D.State a) i = D.State (a + i)

incrementRuleId :: Rule Int b -> Int -> Rule Int b
incrementRuleId (Rule fs b ts) i = Rule (incrementStateId fs i) b (incrementStateId ts i)

incrementGoalStateId :: D.GoalState Int b -> Int -> D.GoalState Int b
incrementGoalStateId (D.GoalState gs p l) i = D.GoalState (incrementStateId gs i) p l

incrementEpsilonNFA :: EpsilonNFA Int b c -> Int -> EpsilonNFA Int b c
incrementEpsilonNFA enfa i = EpsilonNFA {
                              _fstState = incrementedFstState,
                              _currState = incrementedCurrState,
                              _rules = incrementedRules,
                              _goalState = incrementedGoalStates
                            }
  where
    incrementedFstState = incrementStateId (enfa^.fstState) i
    incrementedCurrState = L.map (\cs -> incrementStateId cs i) (enfa^.currState)
    incrementedRules = L.map (\r -> incrementRuleId r i) (enfa^.rules)
    incrementedGoalStates = L.map (\gs -> incrementGoalStateId gs i) (enfa^.goalState)


mergeEpsilonNFA :: [EpsilonNFA Int b c] -> EpsilonNFA Int b c
mergeEpsilonNFA [] = error "mergeEpsilonNFA: can't merge empty Epsilon NFAs"
mergeEpsilonNFA (x:[]) = x
mergeEpsilonNFA enfas@(x:xs) = mkEpsilonNFA (x^.fstState) newRules newGoalState
  where
    stateCounts = L.map (\e -> maximum (L.map (\(Rule _ _ (D.State i)) -> i) (e^.rules))) enfas
    increments = L.scanl1 (+) stateCounts
    incrementedEnfas = x : (L.zipWith (\enfa i -> incrementEpsilonNFA enfa (i + 1)) (tail enfas) increments)
    newEpsilonRules = L.map (\i -> Rule (x^.fstState) (Right Epsilon) (D.State (i + 1))) (init increments)
    newRules = newEpsilonRules ++ (L.concatMap (\e -> (e^.rules)) incrementedEnfas)
    newGoalState = L.concatMap (\e -> (e^.goalState)) incrementedEnfas


{-
- Convert Epsilon-NFA -> DFA
-}

{-
いくつかのルールをグループ化する必要が生まれた。
ある頂点から同一のラベルで遷移する先が複数存在する可能性があるので
ルールどうしをラベルでまとめるという処理が必要
-}

groupedRules :: (Eq b, Ord b) => [ENFARule a b] -> [[ENFARule a b]]
groupedRules rs = L.groupBy (\(Rule _ i1 _) (Rule _ i2 _) -> i1 == i2)
                    $ L.sortBy (\(Rule _ (Left i1) _) (Rule _ (Left i2) _) -> compare i1 i2)
                    $ L.filter (\(Rule _ i _) -> case i of
                                                  (Left _) -> True
                                                  (Right _) -> False) rs

type ENFARule a b = Rule a b
type DFARule a b = D.Rule (Set a) b

genDFAGoalFromDFAState :: (Eq a, Eq b) => [D.State a] -> c -> [D.GoalState a b] -> Maybe (D.GoalState c b)
genDFAGoalFromDFAState as gi gs = if (goalStates == [])
                                              then Nothing
                                              else Just (D.GoalState (D.State gi) goalPriority goalLabel)
  where
    goalStates = L.filter (\(D.GoalState g _ _) -> elem g as) gs
    (D.GoalState _ goalPriority goalLabel) = minimum goalStates

type RuleMap a b = M.Map (D.Input b) [ENFARule a b]

groupeRules :: (Eq b, Ord b) => [ENFARule a b] -> RuleMap a b
groupeRules rs = M.fromList 
                    $ L.map (\xs@((Rule _ (Left i) _):_) -> (i,xs))
                    $ L.groupBy (\(Rule _ i1 _) (Rule _ i2 _) -> i1 == i2)
                    $ L.sortBy (\(Rule _ (Left i1) _) (Rule _ (Left i2) _) -> compare i1 i2)
                    $ L.filter (\(Rule _ i _) -> case i of
                                                  (Left _) -> True
                                                  (Right _) -> False) rs

dfaEdge :: (Eq a, Ord b) => [D.State a] -> RuleMap a b -> [ENFARule a b] -> D.Input b -> [D.State a]
dfaEdge ss rmap rules i = case r of
                      Nothing -> []
                      Just rs -> eclose (L.nub $ L.map (\(Rule _ _ t) -> t) $ 
                                          L.concatMap (\s -> L.filter (\(Rule fs _ _) -> fs == s) rs) ss) rules
  where
    r = M.lookup i rmap

type States a = M.Map Int [D.State a]
type Trans  a = [D.Rule Int a]

generateDFA'' :: (Eq a, Ord b, Eq c) => States a -> Trans b -> [D.GoalState a c] -> [D.GoalState Int c] -> RuleMap a b -> [ENFARule a b] -> D.Input b -> Int -> Int -> (States a, Trans b, [D.GoalState Int c], Int)
generateDFA'' s t g gacc r rules i j p = if (e /= []) 
                                    then case idx of
                                          Just ii -> (s, (D.Rule (D.State j) i (D.State ii)) : t, newGacc, p)
                                          Nothing -> ((M.insert (p + 1) e s), ((D.Rule (D.State j) i (D.State (p + 1))):t), newGacc, (p + 1))
                                    else  (s, t, gacc, p)
  where
    e = dfaEdge (s M.! j) r rules i
    idx = L.find (\idx -> (s M.! idx) == e) [0..p]
    newGoalState = case idx of
                    Just ii -> Nothing
                    Nothing -> genDFAGoalFromDFAState e (p + 1) g 
    newGacc = case newGoalState of
                Just newG -> newG : gacc
                Nothing -> gacc

     
generateDFA' :: (Eq a, Ord b, Eq c) => States a -> Trans b -> [D.GoalState a c] -> [D.GoalState Int c] -> RuleMap a b -> [ENFARule a b] -> [D.Input b] -> Int -> Int -> (Trans b, [D.GoalState Int c])
generateDFA' states trans goals gstates rmap rules alphabets j p 
  | j <= p = generateDFA' newStates newTrans goals newGStates rmap rules alphabets (j + 1) newP
  | otherwise = (trans, gstates)
  where
    (newStates, newTrans, newGStates, newP) = L.foldl (\(s, t, g, p) c -> generateDFA'' s t goals g rmap rules c j p) (states, trans, gstates, p) alphabets

genAlphabets :: (Eq b) => [ENFARule a b] -> [D.Input b]
genAlphabets rs = L.nub $ L.map (\(Rule _ (Left i) _) -> i) $ L.filter (\(Rule _ i _) -> case i of
                                                                                      (Left _) -> True
                                                                                      (Right _) -> False) rs

generateDFA :: (Ord a, Ord b, Eq c) => EpsilonNFA a b c -> (Trans b, [D.GoalState Int c])
generateDFA enfa = generateDFA' states trans (enfa^.goalState) gacc rmap (enfa^.rules) alphabets j p
  where
    fstEclose = eclose [enfa^.fstState] (enfa^.rules)
    states = M.fromList [(0, fstEclose)]
    trans  = []
    rmap = (groupeRules (enfa^.rules))
    alphabets = genAlphabets (enfa^.rules)
    j = 0
    p = 0
    gacc = case (genDFAGoalFromDFAState fstEclose 0 (enfa^.goalState)) of
            Just g -> [g]
            Nothing -> []

genDFA :: (Ord a, Ord b, Eq c) => EpsilonNFA a b c -> D.DFA Int b c
genDFA enfa = D.mkDFA (D.State 0) trans gstates
  where
    (trans, gstates) = generateDFA enfa
