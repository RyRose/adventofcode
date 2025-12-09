import Data.Set qualified as Set
import Data.Text (chunksOf)
import System.Environment (getArgs)

allUnique string = length (Set.fromList string) == length string

findFirst i (x : xs) =
    let
        unique = allUnique (x : take 3 xs)
     in
        if unique then i + 4 else findFirst (i + 1) xs

problem0 string =
    let
        idx = findFirst 0 string
     in
        print idx

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    problem0 contents
