module Main where

import Lib

main :: IO ()
main = print $ ngram 2 "words"
