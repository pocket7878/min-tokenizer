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

initDFA :: D.DFA a b c -> D.DFA a b c
initDFA dfa = dfa {D._currState = (D._fstState dfa)}

tokenizer' :: (Eq b) => D.DFA Int b c -> [(D.Input b)] -> [D.Input b] -> [D.Input b] -> [D.Input b] -> (Maybe ((D.Label c), [D.Input b])) -> [Maybe ((D.Label c), [D.Input b])]
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

tokenizer :: (Eq a, Ord b) => [(a, [R.Reg b])] -> [b] -> [Maybe (D.Label a, [D.Input b])]
tokenizer ts xs = tokenizer' tokenizerDFA inputs inputs inputs [] Nothing
    where
        inputs = map (\x -> (D.Input x)) xs
        tokenModels = map (\((a, rs),p) -> TokenModel (D.Label a) p rs) $ zip ts [0..]
        tokenizerDFA = E.genDFA (buildTokenizerENFA tokenModels)


t1 = ("IF", [R.gets "if"])
t2 = ("ID", [R.and [R.getAny ['h'..'j'], R.getAny ['h'..'j']]])
t3 = ("Error", [R.lone (R.getAny ['a'..'z'])])

tokenizerTest = tokenizer [t1, t2,t3] "iff"
