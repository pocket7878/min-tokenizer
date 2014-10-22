{-# LANGUAGE TemplateHaskell #-}
module DFA where

import qualified Data.Set as S
import Data.List
import Control.Lens
import Control.Monad
import qualified Data.Map as M

import qualified Debug.Trace as Tr
import Text.Show.Functions

data Input a = Input a deriving (Show, Eq, Ord)

data State a = State a deriving (Show, Eq, Ord)

data Rule a b = Rule (State a) (Input b) (State a) deriving (Eq, Show)

matchRule :: (Eq a, Eq b) => Input b -> State a -> [Rule a b] -> Maybe (Rule a b)
matchRule i cs rs = find (\(Rule c l _) -> (cs == c) &&  (i == l)) rs

newtype Label a = Label a deriving (Eq, Show)
type Priority = Int
data GoalState a b = GoalState (State a) Priority (Label b) deriving (Eq, Show)

instance (Eq a, Eq b) => Ord (GoalState a b) where
  compare (GoalState _ p1 _) (GoalState _ p2 _) = compare p1 p2

goalStatep :: (Eq a, Eq b) => State a -> [GoalState a b] -> Bool
goalStatep s gs = any (\(GoalState g _ _) -> s == g) gs

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
    accept' (Just dfa) = goalStatep (dfa^.currState) (dfa^.goalState)

getLabel :: (Eq a) => DFA a b c -> Maybe (Label c)
getLabel dfa = getLabel' matchedGoal
  where
    matchedGoal = filter (\(GoalState gs _ _) -> gs == (dfa^.currState)) (dfa^.goalState)
    getLabel' [] = Nothing
    getLabel' (x:y:xs) = Nothing
    getLabel' ((GoalState _ _ l):[]) = Just l

checkDifference :: (Eq a, Eq b, Ord a) => State a -> State a -> [Input b] -> [Rule a b] -> M.Map (State a, State a) Bool -> Bool
checkDifference s1 s2 is rs etable = any (\i -> case ((nextState s1 i rs), (nextState s2 i rs)) of
                                                            (Nothing, Nothing) -> False
                                                            (Nothing, _) -> True
                                                            (_, Nothing) -> True
                                                            (Just x, Just y) -> if (x == y)
                                                                                  then False
                                                                                  else etable M.! (x, y)) is
  where
    nextState s i rs = case (matchRule i s rs) of
                        Just (Rule _ _ t) -> Just t
                        Nothing -> Nothing

rebuildDFAFromEqualityTable :: (Eq a, Ord a) => DFA a b c -> M.Map (State a, State a) Bool -> DFA Int b c
rebuildDFAFromEqualityTable dfa etable = mkDFA newFstState newRules newGoals
  where
    (newIdTable, _) = M.foldlWithKey
                        (\(itable, i) (s1, s2) b -> 
                            if (not b)
                              then case ((M.lookup s1 itable), (M.lookup s2 itable)) of
                                    (Just x, Just y) -> (M.insert s2 x itable, i)
                                    (Just x, Nothing) -> (M.insert s2 x itable, i)
                                    (Nothing, Just y) -> (M.insert s1 y itable, i)
                                    (Nothing, Nothing) -> (M.insert s1 i (M.insert s2 i itable), i + 1)
                              else case ((M.lookup s1 itable), (M.lookup s2 itable)) of
                                    (Nothing, Nothing) -> (M.insert s1 (i + 1) (M.insert s2 i itable), i + 2)
                                    (Nothing, _) -> (M.insert s1 i itable, i + 1)
                                    (_, Nothing) -> (M.insert s2 i itable, i + 1)
                                    (_, _) -> (itable, i)) (M.empty, 0) etable
    newFstState = case (dfa^.fstState) of
                    i -> (State (newIdTable M.! i))
    newRules = map (\(Rule f i t) -> (Rule (State (newIdTable M.! f)) i (State (newIdTable M.! t)))) (dfa^.rules)
    newGoals = map (\(GoalState g p l) -> (GoalState (State (newIdTable M.! g)) p l)) (dfa^.goalState)


minimize' :: (Eq a, Eq b, Ord a) => DFA a b c -> [Input b] -> M.Map (State a, State a) Bool -> DFA Int b c
minimize' dfa is equalityTable = if updated
                                   then minimize' dfa is newEqualityTable
                                   else rebuildDFAFromEqualityTable dfa newEqualityTable
  where
    (newEqualityTable, updated) = M.foldlWithKey 
                                    (\(ne, u) (s1, s2) b -> 
                                      if (not b)
                                        then let newDiff = (checkDifference s1 s2 is (dfa^.rules) ne) in
                                              if newDiff
                                                then (M.insert (s1, s2) newDiff ne, True)
                                                else (ne, u)
                                        else (ne, u)) (equalityTable, False) equalityTable 

genAlphabets :: (Eq b) => [Rule a b] -> [Input b]
genAlphabets rs = nub $ map (\(Rule _ i _) -> i) rs

allStates dfa = nub $ (dfa^.fstState) : map (\(Rule _ _ t) -> t) (dfa^.rules)

genInitialEtable :: (Eq a, Ord a, Eq c) => DFA a b c -> M.Map (State a, State a) Bool
genInitialEtable dfa = etable
  where
    all = allStates dfa
    etable = M.fromList $ map (\pair@(s1, s2) -> (pair, (goalStatep s1 (dfa^.goalState)) || (goalStatep s2 (dfa^.goalState)))) $ [(x, y) | x <- all, y <- all, x /= y]

minimize :: (Eq a, Eq b, Ord a, Eq c) => DFA a b c -> DFA Int b c
minimize dfa = minimize' dfa (genAlphabets (dfa^.rules)) (genInitialEtable dfa)


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
    putStrLn ((show fs) ++ " [peripheries = 2, label = " ++ (show l) ++ "];")
  });
  (case (dfa^.fstState) of
    (State i) -> do {
      putStrLn ((show i) ++ " [shape = box];");
    });
  putStrLn "}";
}
