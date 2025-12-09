import Data.List (sort)
import Data.Map (elems)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

newtype CD = CD String deriving (Show)
data LS = LS deriving (Show)
newtype Dir = Dir String deriving (Show)
data File = File Integer String deriving (Show)

data Line
    = LineCD CD
    | LineLS LS
    | LineDir Dir
    | LineFile File
    deriving (Show)

parseWords ["$", "cd", dir] = LineCD (CD dir)
parseWords ["$", "ls"] = LineLS LS
parseWords ["dir", dir] = LineDir (Dir dir)
parseWords [size, name] = LineFile (File (read size) name)
parseWords _ = undefined

parseLine line = parseWords (words line)

type Path = [String]
type Sizes = Map.Map Path Integer

data State = State Path Sizes deriving (Show)

increment map key value =
    let
        maybeValue = Map.lookup key map
        newValue = fromMaybe 0 maybeValue + value
     in
        Map.insert key newValue map

incrementAll map [] value = increment map [] value
incrementAll map (x : xs) value =
    let
        newMap = increment map (x : xs) value
     in
        incrementAll newMap xs value

processLine (LineLS LS) accum = accum
processLine (LineCD (CD "/")) (State _ sizes) = State [] sizes
processLine (LineCD (CD "..")) (State (x : xs) sizes) = State xs sizes
processLine (LineCD (CD "..")) (State [] sizes) = State [] sizes
processLine (LineCD (CD dir)) (State path sizes) = State (dir : path) sizes
processLine (LineDir (Dir _)) state = state
processLine (LineFile (File size _)) (State path sizes) = State path (incrementAll sizes path size)

problem0 string =
    let
        ls = reverse $ map parseLine (lines string)
        (State _ msizes) = foldr processLine (State [] Map.empty) ls
        total = 70000000
        desired = 30000000
        used = maximum $ Map.elems msizes
        unused = total - used
        needed = desired - unused
     in
        print $ minimum $ filter (> needed) (Map.elems msizes)

main = do
    path <- getArgs
    contents <- readFile (head path)
    problem0 contents
