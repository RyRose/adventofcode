import Data.Array (Array)
import Data.Array qualified as Array
import Data.Array.Base
import System.Environment (getArgs)

data Direction = U | D | L | R

getValue arr i j = do
    row <- arr !? i
    row !? j

setValue arr i j val = do
    row <- arr !? i
    let newRow = row // [(j, val)]
    return (arr // [(i, newRow)])

nextIndex D i j = (i + 1, j)
nextIndex U i j = (i - 1, j)
nextIndex R i j = (i, j + 1)
nextIndex L i j = (i, j - 1)

markVisible grid mark maxValue dir i j = do
    curValue <- getValue grid i j
    let isVisible = curValue > maxValue
    nextMark <- if isVisible then setValue mark i j True else return mark
    let (nextI, nextJ) = nextIndex dir i j
    let nextMax = max curValue maxValue
    markVisible grid nextMark nextMax dir nextI nextJ

initArray rows cols initialValue =
    array
        (0, rows - 1)
        [ (i, array (0, cols - 1) [(j, initialValue) | j <- [0 .. cols - 1]]) | i <- [0 .. rows - 1]
        ]

-- parseStringInto2DArray :: String -> Array Int (Array Int Int)
parseStringInto2DArray string =
    let
        rows = lines string
        numRows = length rows
        numCols = length (head rows)
        arrRows = map (map read . (: [])) rows :: [[Int]]
        arr = listArray (0, numRows - 1) arrRows
     in
        arrRows

problem0 string =
    let
        input = parseStringInto2DArray string
     in
        print input

main = do
    path <- getArgs
    contents <- readFile (head path)
    problem0 contents
