import Data.List (sort)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import Text.Read (readMaybe)

groupInts :: Maybe Integer -> [[Integer]] -> [[Integer]]
groupInts Nothing acc = [] : acc
groupInts (Just i) (x : xs) = (i : x) : xs

problem0 :: String -> Integer
problem0 string =
        let
                maybeInts = map readMaybe (lines string)
                groups = foldr groupInts [[]] maybeInts
                sums = map sum groups
                sortedSums = reverse (sort sums)
         in
                sum (take 3 sortedSums)

main :: IO ()
main = do
        path <- getArgs
        contents <- readFile (head path)
        print (problem0 contents)
