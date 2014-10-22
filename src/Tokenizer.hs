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

data TokenModel a = TokenModel (D.Label a) D.Priority [R.Reg] deriving (Eq, Show)

buildTokenModelENFA :: TokenModel a -> E.EpsilonNFA Int Char a
buildTokenModelENFA (TokenModel l p rs) = B.buildNFA (R.or rs) p l

buildTokenizerENFA :: (Eq a) => [TokenModel a] -> E.EpsilonNFA Int Char a
buildTokenizerENFA ts = mergedEnfa
  where
    enfas = map (\t -> buildTokenModelENFA t) ts
    mergedEnfa = E.mergeEpsilonNFA enfas

initDFA :: D.DFA a b c -> D.DFA a b c
initDFA dfa = dfa {D._currState = (D._fstState dfa)}

newtype Tokenizer a = Tokenizer (D.DFA Int Char a)
buildTokenizer :: (Eq a) => [(a, [R.Reg])] -> Tokenizer a
buildTokenizer ts = Tokenizer tokenizerDFA
    where
        tokenModels = map (\((a, rs),p) -> TokenModel (D.Label a) p rs) $ zip ts [0..]
        tokenizerDFA = D.minimize $ E.genDFA (buildTokenizerENFA tokenModels)

-- MatchingResult type: use left for correct matching, use right for error tokens.
type TemporaryMatch a b = Maybe  ((D.Label a), [D.Input b])
type MatchingResult a b = Either ((D.Label a), [D.Input b]) (D.Input b)

tokenization' :: (Eq b) => D.DFA Int b c -> [(D.Input b)] -> [D.Input b] -> [D.Input b] -> [D.Input b] -> TemporaryMatch c b -> [MatchingResult c b]
tokenization' _ [] _ _ _ lastLabel = case lastLabel of
                                Just l -> [Left l]
                                Nothing -> []
tokenization' _ _ _ [] _ lastLabel = case lastLabel of
                                Just l -> [Left l]
                                Nothing -> []
tokenization' dfa xs top bottom@(i:is) acc lastLabel = case newDFA of
                                                Nothing -> case lastLabel of
                                                            (Just l) -> Left l  : (tokenization' (initDFA dfa) top top top [] Nothing)
                                                            Nothing  -> Right i : (tokenization' (initDFA dfa) newBottom newBottom newBottom [] Nothing)
                                                Just ndfa -> case goalLabel of
                                                    Just l -> tokenization' ndfa xs newBottom newBottom newAcc (Just (l, newAcc))
                                                    Nothing -> tokenization' ndfa xs top newBottom newAcc lastLabel
  where
    newAcc = acc ++ [i]
    newBottom = is
    newDFA = D.updateDFA dfa i
    goalLabel = D.getLabel $ M.fromJust newDFA

tokenization :: (Eq a) => Tokenizer a -> String -> [MatchingResult a Char]
tokenization (Tokenizer dfa) xs = tokenization' dfa inputs inputs inputs [] Nothing
    where
        inputs = map (\x -> (D.Input x)) xs

t1 = ("IF", [R.gets "if"])
t2 = ("ID", [R.some (R.oneOf ['a'..'z'])])

tokenizer = buildTokenizer [
                ("IF", [R.gets "if"]),
                ("ID", [R.and [R.alphabetCls, R.star R.alnumCls]]),
                ("NUM", [R.some R.digitCls]),
                ("REAL", [R.or [R.and [R.some R.digitCls, R.get '.', R.star R.digitCls], R.and [R.star R.digitCls, R.get '.', R.some R.digitCls]]]),
                ("SPACE", [R.or [R.and [R.gets "--", R.star R.alphabetCls, R.get '\n'], R.some (R.or [R.get ' ', R.get '\n', R.get '\t'])]])]

tokenizerTest = tokenization tokenizer
