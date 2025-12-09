import Data.Maybe (catMaybes, isJust)
import Data.Text (splitOn)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Rock > Scissors > Paper > Rock

data Move = Rock | Paper | Scissors deriving (Eq, Show)

parseMove :: String -> Maybe Move
parseMove "A" = Just Rock
parseMove "B" = Just Paper
parseMove "C" = Just Scissors
parseMove "X" = Just Rock
parseMove "Y" = Just Paper
parseMove "Z" = Just Scissors
parseMove _ = Nothing

data Matchup = Matchup Move Move deriving (Eq, Show)

parseMatchup :: [String] -> Maybe Matchup
parseMatchup [a, b] = do
    moveA <- parseMove a
    moveB <- parseMove b
    Just (Matchup moveA moveB)
parseMatchup _ = Nothing

parseLine :: String -> Maybe Matchup
parseLine s = parseMatchup (words s)

data MatchStatus = Win | Lose | Draw deriving (Eq, Show)

matchStatus :: Matchup -> MatchStatus
matchStatus (Matchup a b) | a == b = Draw
matchStatus (Matchup Rock Scissors) = Lose
matchStatus (Matchup Scissors Rock) = Win
matchStatus (Matchup Paper Rock) = Lose
matchStatus (Matchup Rock Paper) = Win
matchStatus (Matchup Scissors Paper) = Lose
matchStatus (Matchup Paper Scissors) = Win

scoreMove :: Move -> Integer
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scoreMatchup :: Matchup -> Integer
scoreMatchup (Matchup a b) =
    scoreMove b
        + case matchStatus (Matchup a b) of
            Draw -> 3
            Win -> 6
            Lose -> 0

problem0 :: String -> Integer
problem0 string =
    let
        maybeMatchups = map parseLine (lines string)
        matchups = catMaybes maybeMatchups
        scores = map scoreMatchup matchups
     in
        sum scores

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    print (problem0 contents)
