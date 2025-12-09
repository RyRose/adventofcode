import Data.Char (isLower)
import Data.Set qualified as Set
import GHC.Base (ord)
import System.Environment (getArgs)

commonItem ls =
    let
        sets = map Set.fromList ls
        common = foldr Set.intersection (head sets) (tail sets)
     in
        head (Set.toList common)

charPriority c
    | isLower c = toInteger (ord c - ord 'a') + 1
    | otherwise = toInteger (ord c - ord 'A') + 26 + 1

combineGroups line (x : xs)
    | length x == 3 = [line] : (x : xs)
    | otherwise = (line : x) : xs

problem0 string =
    let
        groups = foldr combineGroups [[]] (lines string)
        commonItems = map commonItem groups
        priorities = map charPriority commonItems
     in
        print (sum priorities)

main = do
    args <- getArgs
    contents <- readFile (head args)
    problem0 contents
