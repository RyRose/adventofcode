{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isLower)
import Data.Set qualified as Set
import Data.Text (pack, split, splitOn, unpack)
import GHC.Base (ord)
import System.Environment (getArgs)

data Range = Range Integer Integer deriving (Show, Eq)

parseRange s =
    let
        [a, b] = map (read . unpack) (splitOn "-" s)
     in
        Range a b

rangeContains (Range l0 r0) (Range l1 r1) = l0 <= l1 && r1 <= r0

data Line = Line Range Range deriving (Show, Eq)

parseLine line =
    let
        [a, b] = splitOn "," (pack line)
     in
        Line (parseRange a) (parseRange b)

lineValid (Line r0 r1) = rangeContains r0 r1 || rangeContains r1 r0

problem0 :: String -> IO ()
problem0 string =
    let
        allLines = map parseLine (lines string)
        valids = map lineValid allLines
     in
        print (length (filter id valids))

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    problem0 contents
