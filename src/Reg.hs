{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reg where

import  Text.Show.Functions
import  qualified Data.List as L

data RegTerm a = Is a | Bracket [a] deriving (Eq)
data RegExp  a = Or [Either (RegTerm a) (RegExp a)] | And [Either (RegTerm a) (RegExp a)] | Star (Either (RegTerm a) (RegExp a)) | Lone (Either (RegTerm a) (RegExp a)) | Some (Either (RegTerm a) (RegExp a)) deriving (Eq)
type Reg a = (Either (RegTerm a) (RegExp a))

star :: Reg a -> Reg a
star a = (Right (Star a))

lone :: Reg a -> Reg a
lone a = (Right (Lone a))

some :: Reg a -> Reg a
some a = (Right (Some a))

and :: [Reg a] -> Reg a
and rs = (Right (And rs))
(&&) :: [Reg a] -> Reg a
(&&) = Reg.and

or :: [Reg a] -> Reg a
or rs = (Right (Or rs))
(||) :: [Reg a] -> Reg a
(||) = Reg.or

get :: (Eq a) => a -> Reg a
get a = (Left (Is a))

gets :: (Eq a) => [a] -> Reg a
gets xs = (Right (And (map get xs)))

getAny :: (Eq a) => [a] -> Reg a
getAny xs = (Left (Bracket xs))

instance Show a => Show (RegTerm a) where
    show (Is x) = show x
    show (Bracket xs) = "[" ++ L.concatMap (\x -> (show x)) xs ++ "]"
instance Show a => Show (RegExp a) where
    show (Or xs) = L.intercalate "+" (map (\x -> (show x)) xs)
    show (And xs) = L.intercalate "" (map (\x -> (show x)) xs)
    show (Star a) = (show a) ++ "*"
    show (Lone a) = (show a) ++ "?"
    show (Some a) = (show a) ++ "+"

instance Show a => Show (Reg a) where
    show (Left a) = show a
    show (Right a) = "(" ++ show a ++ ")"
