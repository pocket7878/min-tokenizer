{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reg where

import  Text.Show.Functions
import  qualified Data.List as L

data RegTerm = Is Char | OneOf [Char] | NoneOf [Char] | Any deriving (Eq)
data RegExp  = Or [Either RegTerm RegExp] | 
                And [Either RegTerm RegExp] | 
                Star (Either RegTerm RegExp) | 
                Lone (Either RegTerm RegExp) | 
                Some (Either RegTerm RegExp) deriving (Eq)
type Reg = (Either RegTerm RegExp)

star :: Reg -> Reg
star a = (Right (Star a))

lone :: Reg -> Reg 
lone a = (Right (Lone a))

some :: Reg -> Reg
some a = (Right (Some a))

and :: [Reg] -> Reg
and rs = (Right (And rs))
(&&) :: [Reg] -> Reg
(&&) = Reg.and

or :: [Reg] -> Reg
or rs = (Right (Or rs))
(||) :: [Reg] -> Reg
(||) = Reg.or

get :: Char -> Reg
get a = (Left (Is a))

gets :: [Char] -> Reg
gets xs = (Right (And (map get xs)))

oneOf :: [Char] -> Reg
oneOf xs = (Left (OneOf xs))

noneOf :: [Char] -> Reg
noneOf xs = (Left (NoneOf xs))

any :: Reg
any = (Left Any)

alphabetCls :: Reg
alphabetCls = (Left (OneOf ['a'..'z']))

digitCls :: Reg
digitCls = (Left (OneOf ['0'..'9']))

alnumCls :: Reg
alnumCls = (Left (OneOf (['a'..'z']++['0'..'9'])))

instance Show RegTerm where
    show (Is x) = [x]
    show (OneOf xs) = "[" ++ xs ++ "]"
    show (NoneOf xs) = "[^" ++ xs ++ "]"
    show Any = "."

instance Show RegExp where
    show (Or xs) = L.intercalate "+" (map (\x -> (show x)) xs)
    show (And xs) = L.intercalate "" (map (\x -> (show x)) xs)
    show (Star a) = (show a) ++ "*"
    show (Lone a) = (show a) ++ "?"
    show (Some a) = (show a) ++ "+"

instance Show Reg where
    show (Left a) = show a
    show (Right a) = "(" ++ show a ++ ")"
