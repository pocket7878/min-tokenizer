import Test.Hspec

import qualified Reg as R
import qualified EpsilonNFA as N
import qualified DFA as D
import qualified NFABuilder as B
import qualified Control.Monad as M

regTest :: (Eq a) => R.Reg a -> [([a],Bool)] -> IO ()
regTest reg cases = M.mapM_ (\(is, res) -> do {N.accept enfa is `shouldBe` res;D.accept dfa is `shouldBe` res;}) cases
     where
       enfa = B.buildNFA reg
       dfa = N.genDFA enfa

main :: IO ()
main = hspec $ do
  describe "Simple Regex" $ do
    it "single" $ regTest   (R.get 'a')
      [("", False), ("a", True), ("b", False)]
    it "and"    $ regTest   (R.and [(R.get 'a'), (R.get 'b')])
      [("", False), ("a", False), ("b", False), ("ab", True), ("abc", False)]
    it "or"     $ regTest   (R.or [(R.get 'a'), (R.get 'b')])
      [("", False), ("a", True), ("b", True), ("ab", False)]
    it "star"   $ regTest   (R.star (R.get 'a'))
      [("", True),  ("a", True), ("aa", True), ("b", False), ("ab", False)]
    it "lone"   $ regTest   (R.lone (R.get 'a'))
      [("", True),  ("a", True), ("aa", False), ("b", False)]
    it "some"   $ regTest   (R.some (R.get 'a'))
      [("", False), ("a", True), ("aa", True), ("aaa", True), ("ab", False)]
  describe "Complex And Regex" $ do
    it "and-or"         $ regTest   (R.and [(R.or [(R.get 'a'), (R.get 'b')]), (R.get 'c')])
      [("", False), ("a", False), ("b", False), ("ab", False), ("ac", True), ("bc", True)]
    it "and-star"       $ regTest   (R.and [(R.get 'a'), (R.star (R.get 'b'))])
      [("", False),  ("a", True), ("aa", False), ("ab", True), ("abb", True)]
    it "and-lone"       $ regTest   (R.and [(R.get 'a'), (R.lone (R.get 'b'))])
      [("", False),  ("a", True), ("ab", True), ("abb", False)]
    it "and-some"       $ regTest   (R.and [(R.get 'a'), (R.some (R.get 'b'))])
      [("", False), ("a", False), ("ab", True), ("abb", True), ("abbb", True)]
  describe "Complex Or Regex" $ do
    it "or-and"        $ regTest   (R.or [(R.and [(R.get 'a'), (R.get 'b')]), (R.get 'c')])
      [("", False), ("a", False), ("b", False), ("ab", True), ("c", True)]
    it "or-or"         $ regTest   (R.or [(R.or [(R.get 'a'), (R.get 'b')]), (R.get 'c')])
      [("", False), ("a", True), ("b", True), ("c", True)]
    it "or-star"       $ regTest   (R.or [(R.get 'a'), (R.star (R.get 'b'))])
      [("", True),  ("a", True), ("aa", False), ("b", True), ("bb", True)]
    it "or-lone"       $ regTest   (R.or [(R.get 'a'), (R.lone (R.get 'b'))])
      [("", True),  ("a", True), ("b", True)]
    it "or-some"       $ regTest   (R.or [(R.get 'a'), (R.some (R.get 'b'))])
      [("", False), ("a", True), ("b", True), ("bb", True), ("bbb", True)]
  describe "Complex Star Regex" $ do
    it "star-and"      $ regTest   (R.star (R.and [(R.get 'a'), (R.get 'b')]))
      [("", True), ("a", False), ("b", False), ("ab", True), ("aba", False), ("abb", False), ("abab", True)]
    it "star-or"       $ regTest   (R.star (R.or [(R.get 'a'), (R.get 'b')]))
      [("", True), ("a", True), ("b", True), ("aa", True), ("ab", True), ("aba", True), ("abb", True)]
    it "star-star"     $ regTest   (R.star (R.star (R.get 'a')))
      [("", True),  ("a", True), ("aa", True), ("aa", True)]
    it "star-lone"     $ regTest   (R.star (R.lone (R.get 'a')))
      [("", True),  ("a", True), ("aa", True), ("aaa", True)]
    it "star-some"     $ regTest   (R.star (R.some (R.get 'a')))
      [("", True), ("a", True), ("aa", True), ("aaa", True)]
  describe "Complex Lone Regex" $ do
    it "lone-and"      $ regTest   (R.lone (R.and [(R.get 'a'), (R.get 'b')]))
      [("", True), ("ab", True), ("abab", False)]
    it "lone-or"       $ regTest   (R.lone (R.or [(R.get 'a'), (R.get 'b')]))
      [("", True), ("a", True), ("b", True), ("aa", False), ("ab", False)]
    it "lone-star"     $ regTest   (R.lone (R.star (R.get 'a')))
      [("", True),  ("a", True), ("aa", True), ("aaa", True)]
    it "lone-lone"     $ regTest   (R.lone (R.lone (R.get 'a')))
      [("", True),  ("a", True), ("aa", False)]
    it "lone-some"     $ regTest   (R.lone (R.some (R.get 'a')))
      [("", True), ("a", True), ("aa", True), ("aaa", True)]
  describe "Complex Some Regex" $ do
    it "some-and"      $ regTest   (R.some (R.and [(R.get 'a'), (R.get 'b')]))
      [("", False), ("ab", True), ("abab", True), ("ababab", True)]
    it "some-or"       $ regTest   (R.some (R.or [(R.get 'a'), (R.get 'b')]))
      [("", False), ("a", True), ("b", True), ("aa", True), ("ab", True), ("aba", True), ("abb", True)]
    it "some-star"     $ regTest   (R.some (R.star (R.get 'a')))
      [("", True),  ("a", True), ("aa", True), ("aaa", True)]
    it "some-lone"     $ regTest   (R.some (R.lone (R.get 'a')))
      [("", True),  ("a", True), ("aa", True), ("aaa", True)]
    it "some-some"     $ regTest   (R.some (R.some (R.get 'a')))
      [("", False), ("a", True), ("aa", True), ("aaa", True)]
