import Data.Maybe (catMaybes, isJust)
import Data.Text (splitOn)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Move = Rock | Paper | Scissors deriving (Eq, Show)

parseMove :: String -> Maybe Move
parseMove "A" = Just Rock
parseMove "B" = Just Paper
parseMove "C" = Just Scissors
parseMove _ = Nothing

data PreMatchup = PreMatchup Move MatchStatus deriving (Eq, Show)

parsePreMatchup :: [String] -> Maybe PreMatchup
parsePreMatchup [a, b] = do
    move <- parseMove a
    status <- parseMatchStatus b
    Just (PreMatchup move status)
parsePreMatchup _ = Nothing

data Matchup = Matchup Move Move deriving (Eq, Show)

parseMatchup :: [String] -> Maybe Matchup
parseMatchup [a, b] = do
    moveA <- parseMove a
    moveB <- parseMove b
    Just (Matchup moveA moveB)
parseMatchup _ = Nothing

parseLine :: String -> Maybe PreMatchup
parseLine s = parsePreMatchup (words s)

data MatchStatus = Win | Lose | Draw deriving (Eq, Show)

parseMatchStatus :: String -> Maybe MatchStatus
parseMatchStatus "X" = Just Lose
parseMatchStatus "Y" = Just Draw
parseMatchStatus "Z" = Just Win
parseMatchStatus _ = Nothing

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

convPrematchup :: PreMatchup -> Matchup
convPrematchup (PreMatchup a Draw) = Matchup a a
convPrematchup (PreMatchup Rock Win) = Matchup Rock Paper
convPrematchup (PreMatchup Rock Lose) = Matchup Rock Scissors
convPrematchup (PreMatchup Paper Win) = Matchup Paper Scissors
convPrematchup (PreMatchup Paper Lose) = Matchup Paper Rock
convPrematchup (PreMatchup Scissors Win) = Matchup Scissors Rock
convPrematchup (PreMatchup Scissors Lose) = Matchup Scissors Paper

problem0 :: String -> Integer
problem0 string =
    let
        maybeMatchups = map parseLine (lines string)
        preMatchups = catMaybes maybeMatchups
        matchups = map convPrematchup preMatchups
        scores = map scoreMatchup matchups
     in
        sum scores

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    print (problem0 contents)
