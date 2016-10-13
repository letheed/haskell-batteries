module Data.List.Batteries
  ( readLst
  , lengthIs, lengthLT, lengthGT, lengthLET, lengthGET
  , alone, some, several
  , headM, lastN
  , picks
  , groupOn, splitOn
  , dedup, elemOrd, maximumOn, sortGroupOn
  , combinationsOf, permutations'
  ) where

import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

readLst :: (Read a) => String -> [a]
readLst = read . (\str -> '[' : str ++ "]")

lengthIs :: Int -> [a] -> Bool
lengthIs n = (n ==) . length

lengthLT :: Int -> [a] -> Bool
lengthLT n = null . drop (n-1)

lengthGT :: Int -> [a] -> Bool
lengthGT n = not . null . drop n

lengthLET :: Int -> [a] -> Bool
lengthLET n = null . drop n

lengthGET :: Int -> [a] -> Bool
lengthGET n = not . null . drop (n-1)

alone :: [a] -> Bool
alone = null . drop 1

some :: [a] -> Bool
some = not . null

several :: [a] -> Bool
several = not . alone

headM :: [a] -> Maybe a
headM = listToMaybe

lastN :: Int -> [a] -> [a]
lastN n = foldl' (const . tail) <*> drop n

picks :: [a] -> [(a, [a])]
picks []     = []
picks (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- picks xs]

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = let memo x = let y = f x in y `seq` (y, x)
            in map (map snd) . groupBy ((==) `on` fst) . map memo

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ []     = []
splitOn c phrase = word : splitOn c (drop 1 rest)
  where (word, rest) = break (c ==) phrase

dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort

elemOrd :: (Ord a) => a -> [a] -> Bool
elemOrd e lst
  | null ge   = False
  | otherwise = e == head ge
  where ge = dropWhile (e >) lst

maximumOn :: (Ord b) => (a -> b) -> [a] -> a
maximumOn f = let memo x = let y = f x in y `seq` (y, x)
              in snd . maximumBy (comparing fst) . map memo

sortGroupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortGroupOn f = let memo x = let y = f x in y `seq` (y, x)
                in map (map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst) . map memo

combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 0 _      = [[]]
combinationsOf _ []     = []
combinationsOf n (x:xs) = fmap (x:) (combinationsOf (n-1) xs) ++ combinationsOf n xs

permutations' :: (Eq a) => [a] -> [[a]]
{-# SPECIALIZE permutations' :: [Int] -> [[Int]] #-}
{-# SPECIALIZE permutations' :: String -> [String] #-}
permutations' [] = [[]]
permutations' xs = concatMap (\x -> map (x:) . permutations' $ delete x xs) xs
