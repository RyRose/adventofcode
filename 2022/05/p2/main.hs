{-# LANGUAGE OverloadedStrings #-}

import Data.Array (Array)
import Data.Array.Base
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (chunksOf, index, pack)
import System.Environment (getArgs)

newtype Stack = Stack String deriving (Show, Eq)

pop (Stack (x : xs)) = (Just x, Stack xs)
pop (Stack []) = (Nothing, Stack [])

popN n (Stack xs) =
    let
        (popped, stack) = splitAt n xs
     in
        (popped, Stack stack)

push ' ' (Stack xs) = Stack xs
push x (Stack xs) = Stack (x : xs)

pushN xs (Stack ys) = Stack (xs ++ ys)

newtype Stacks = Stacks (Array Integer Stack) deriving (Show, Eq)

stacksMessage (Stacks arr) =
    let
        pops = map pop (elems arr)
        maybeChars = map fst pops
     in
        catMaybes maybeChars

move (Stacks stacks) num i j =
    let
        fromStack = stacks ! i
        toStack = stacks ! j
        (toMove, newFromStack) = popN num fromStack
        newToStack = pushN toMove toStack
        newStacks = stacks // [(i, newFromStack), (j, newToStack)]
     in
        Stacks newStacks

parseStackLine line =
    let
        chunks = chunksOf 4 line
        letters = map (`index` 1) chunks
     in
        letters

parseStackLines lines =
    let
        parsedLines = reverse (map parseStackLine lines)
        numStacks = length (last parsedLines)
        paddedLines = map (++ repeat ' ') parsedLines
        initialStacks = [Stack [] | _ <- [1 .. numStacks]]
        finalStacks = foldr (zipWith push) initialStacks paddedLines
     in
        Stacks (listArray (1, fromIntegral numStacks) finalStacks)

data Move = Move Integer Integer Integer deriving (Show, Eq)

parseMoveLine line =
    let
        wordsLine = words line
        num = read (wordsLine !! 1)
        from = read (wordsLine !! 3)
        to = read (wordsLine !! 5)
     in
        Move num from to

makeMove (Move num from to) stacks = move stacks (fromInteger num) from to

problem0 string =
    let
        (stackLines, moveLines) = span (/= "") (lines string)
        stacks = parseStackLines (map pack (tail (reverse stackLines)))
        moves = reverse (map parseMoveLine (tail moveLines))
        finalStacks = foldr makeMove stacks moves
        message = stacksMessage finalStacks
     in
        print message

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    problem0 contents
