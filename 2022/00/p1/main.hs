import System.Environment (getArgs)

problem0 string =
    let
        example = "foo"
     in
        print example

main = do
    path <- getArgs
    contents <- readFile (head path)
    problem0 contents
