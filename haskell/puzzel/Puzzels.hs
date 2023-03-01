{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Puzzels where
length' :: [a] -> Int
length' = foldr (\_ l -> 1+l) 0

or' :: [Bool] -> Bool
or' = or

elem' :: Eq a => a -> [a] -> Bool
elem' x y = foldr (\curr acc -> acc || curr == x) False y

map' :: (a -> b) -> [a] -> [b]
map' x y = foldr (\curr acc -> x curr : acc) [] y

reverseR :: [a] -> [a]
reverseR x = foldr (\a b c -> b(a:c)) id x []

reverseL :: [a] -> [a]
reverseL = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverseR x

plusplus :: [a] -> [a] -> [a]
plusplus x y = foldr (\curr acc -> curr : acc) y x

fibonacci :: Num a => [a]
fibonacci = 0 : scanl (+) 1 fibonacci