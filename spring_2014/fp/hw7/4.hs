import System.Random

main = do
    goal <- randomRIO (1, 100)
    go goal 0

go :: Int -> Int -> IO ()
go goal 5 = do
    putStrLn "You lost!"
go goal try = do
    guessStr <- getLine
    check (read guessStr) 
    where check guess | guess < goal = do putStrLn "Greater"; go goal (try + 1)
                      | guess > goal = do putStrLn "Less"; go goal (try + 1)
                      | guess == goal = do putStrLn "You won!"; return ()
