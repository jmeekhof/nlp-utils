module Lib where

import Data.Char
import qualified Data.Set as Set

ngram :: Int -> [a] -> [[a]]
ngram n xs
  | n <= length xs = take n xs : ngram n (drop 1 xs)
  | otherwise = []

bigram :: [a] -> [[a]]
bigram = ngram 2

trigram :: [a] -> [[a]]
trigram = ngram 3

jaccard :: Ord a => Set.Set a -> Set.Set a -> Float
jaccard source cible =
  fromIntegral (Set.size inters) / fromIntegral (Set.size un)
  where
    inters = Set.intersection source cible
    un = Set.union source cible

jaccardBy :: Ord a => Int -> [a] -> [a] -> Float
jaccardBy n a m = jaccard (Set.fromList (ngram n a)) (Set.fromList (ngram n m))

jaccardBiGram :: Ord a => [a] -> [a] -> Float
jaccardBiGram source cible = jaccardBy 2 source cible

jaccardTriGram :: Ord a => [a] -> [a] -> Float
jaccardTriGram source cible = jaccardBy 3 source cible
