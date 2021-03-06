{-# LANGUAGE TemplateHaskell #-}
module NFABuilder where

import qualified Debug.Trace as Tr
import qualified DFA as D (Input(Input), State(State), GoalState(GoalState), Label(Label), Priority)
import qualified EpsilonNFA as N (Epsilon(Epsilon), Rule(Rule), EpsilonNFA(EpsilonNFA), mkEpsilonNFA)
import qualified Reg as R (Reg, RegTerm(Is, OneOf, NoneOf, Any), RegExp(Or, And, Star, Lone, Some))
import qualified Data.CharSet as C(toList, fromList, (\\))
import qualified Data.CharSet.Posix.Ascii as A (print)

buildNFA' :: R.Reg -> ([N.Rule Int Char], Int) -> ([N.Rule Int Char], Int)

buildNFA' (Left (R.Is a)) (acc, s) = (newRule : acc, (s + 1))
  where
      newRule = N.Rule (D.State s) (Left (D.Input a)) (D.State (s + 1))

buildNFA' (Left (R.OneOf xs)) st@(acc, s) = (newAcc, s + 2)
    where
      tmpAcc = map (\i -> N.Rule (D.State (s + 1)) (Left (D.Input i)) (D.State (s + 2))) xs
      newAcc = (N.Rule (D.State s) (Right (N.Epsilon)) (D.State (s + 1))) : tmpAcc ++ acc

buildNFA' (Left (R.NoneOf xs)) st@(acc, s) = buildNFA' 
    (Left (R.OneOf (C.toList (A.print C.\\ C.fromList xs)))) st

buildNFA' (Left (R.Any)) st@(acc, s) = buildNFA'
    (Left (R.OneOf (C.toList A.print))) st

buildNFA' (Right (R.Or xs)) st@(acc, s) = (newAcc, last_i + 1)
  where
    ((tmpRuleAcc, last_i), iacc) = foldl (\((acc, i), iacc) x -> (buildNFA' x (acc, i + 1),(i + 1):iacc)) ((acc, s),[]) xs
    epRules = (map (\i -> N.Rule (D.State s) (Right (N.Epsilon)) (D.State i)) iacc)
                ++ (map (\i -> N.Rule (D.State (i - 1)) (Right (N.Epsilon)) (D.State (last_i + 1))) (init iacc))
                ++ [N.Rule (D.State last_i) (Right N.Epsilon) (D.State (last_i + 1))]
    newAcc = epRules ++ tmpRuleAcc

buildNFA' (Right (R.And xs)) st@(acc, s) = (newAcc, last_i)
  where
    (newAcc,last_i) = foldl (\(acc, i) x -> buildNFA' x (acc, i)) (acc, s) xs

buildNFA' (Right (R.Star a)) st@(acc, s) = (newAcc, al + 1)
  where
    (acc1, al) = buildNFA' a (acc, s + 1)
    epRule1 = N.Rule (D.State s) (Right N.Epsilon) (D.State (s + 1))
    epRule2 = N.Rule (D.State al) (Right N.Epsilon) (D.State (s + 1))
    epRule3 = N.Rule (D.State al) (Right N.Epsilon) (D.State (al + 1))
    epRule4 = N.Rule (D.State s) (Right N.Epsilon) (D.State (al + 1))
    newAcc = [epRule1, epRule2, epRule3, epRule4] ++ acc1

buildNFA' (Right (R.Lone a)) st@(acc, s) = (newAcc, al + 1)
  where
    (acc1, al) = buildNFA' a (acc, s + 1)
    epRule1 = N.Rule (D.State s) (Right N.Epsilon) (D.State (s + 1))
    epRule2 = N.Rule (D.State al) (Right N.Epsilon) (D.State (al + 1))
    epRule3 = N.Rule (D.State s) (Right N.Epsilon) (D.State (al + 1))
    newAcc = [epRule1, epRule2, epRule3] ++ acc1

buildNFA' (Right (R.Some a)) st@(acc, s) = (newAcc, al + 1)
  where
    (acc1, al) = buildNFA' a (acc, s + 1)
    epRule1 = N.Rule (D.State s) (Right N.Epsilon) (D.State (s + 1))
    epRule3 = N.Rule (D.State al) (Right N.Epsilon) (D.State (s + 1))
    epRule2 = N.Rule (D.State al) (Right N.Epsilon) (D.State (al + 1))
    newAcc = [epRule1, epRule2, epRule3] ++ acc1

buildNFA :: R.Reg -> D.Priority -> D.Label b -> N.EpsilonNFA Int Char b
buildNFA reg p l = N.mkEpsilonNFA (D.State 0) rules [(D.GoalState (D.State last) p l)]
  where
    (rules, last) = buildNFA' reg ([], 0)
