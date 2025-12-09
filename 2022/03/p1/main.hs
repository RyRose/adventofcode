import Data.Char (isLower)
import Data.Set qualified as Set
import GHC.Base (ord)
import System.Environment (getArgs)

data Compartment = Compartment String String deriving (Eq, Show)

parseLine :: String -> Compartment
parseLine s =
    let
        (first, second) = splitAt (length s `div` 2) s
     in
        Compartment first second

compartmentCommonItem :: Compartment -> Char
compartmentCommonItem (Compartment l r) =
    let
        lset = Set.fromList l
        rset = Set.fromList r
        common = Set.intersection lset rset
     in
        head (Set.toList common)

charPriority c
    | isLower c = toInteger (ord c - ord 'a') + 1
    | otherwise = toInteger (ord c - ord 'A') + 26 + 1

compartmentPriority :: Compartment -> Integer
compartmentPriority c = charPriority (compartmentCommonItem c)

problem0 :: String -> IO ()
problem0 string =
    let
        compartments = map parseLine (lines string)
        priorities = map compartmentPriority compartments
     in
        print (sum priorities)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    problem0 contents
