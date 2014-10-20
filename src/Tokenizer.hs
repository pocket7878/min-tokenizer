{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tokenizer where

import qualified Data.Set as S
import qualified Reg as R
import qualified NFABuilder as B
import qualified EpsilonNFA as E
import qualified DFA as D

import qualified Data.Maybe as M

import qualified Debug.Trace as Tr

data TokenModel a b = TokenModel (D.Label b) D.Priority [R.Reg a] deriving (Eq, Show)

buildTokenModelENFA :: (Eq b) => TokenModel b c -> E.EpsilonNFA Int b c
buildTokenModelENFA (TokenModel l p rs) = B.buildNFA (R.or rs) p l

buildTokenizerENFA :: (Eq b, Eq c) => [TokenModel b c] -> E.EpsilonNFA Int b c
buildTokenizerENFA ts = mergedEnfa
  where
    enfas = map (\t -> buildTokenModelENFA t) ts
    mergedEnfa = E.mergeEpsilonNFA enfas

initDFA :: D.DFA (S.Set Int) b c -> D.DFA (S.Set Int) b c
initDFA dfa = dfa {D._currState = (D._fstState dfa)}

tokenizer' :: (Eq b) => D.DFA (S.Set Int) b c -> [(D.Input b)] -> [D.Input b] -> [D.Input b] -> [D.Input b] -> (Maybe ((D.Label c), [D.Input b])) -> [Maybe ((D.Label c), [D.Input b])]
tokenizer' _ [] _ _ _ lastLabel = case lastLabel of
                                Just l -> [Just l]
                                Nothing -> []
tokenizer' _ _ _ [] _ lastLabel = case lastLabel of
                                Just l -> [Just l]
                                Nothing -> []
tokenizer' dfa xs top bottom@(i:is) acc lastLabel = case newDFA of
                                                Nothing -> lastLabel : (tokenizer' (initDFA dfa) top top top [] Nothing)
                                                Just ndfa -> case goalLabel of
                                                    Just l -> tokenizer' ndfa xs newBottom newBottom newAcc (Just (l, newAcc))
                                                    Nothing -> tokenizer' ndfa xs top newBottom newAcc lastLabel
  where
    newAcc = acc ++ [i]
    newBottom = is
    newDFA = D.updateDFA dfa i
    goalLabel = D.getLabel $ M.fromJust newDFA

tokenizer :: [(a, [R.Reg b])] -> [Maybe (D.Label a)]
tokenizer ts = undefined


t1 = TokenModel (D.Label "IF") 1 [R.gets "if"]
t2 = TokenModel (D.Label "ID") 0 [R.and [R.getAny ['h'..'j'], R.getAny ['h'..'j']]]
enfa =buildTokenizerENFA [t1, t2]
dfa = E.genDFA enfa

t3 = TokenModel (D.Label "Any") 1 [R.and [R.star (R.getAny ['h'..'j']), R.getAny ['h'..'j']]]
dfa2 = E.genDFA (buildTokenizerENFA [t1, t3])
