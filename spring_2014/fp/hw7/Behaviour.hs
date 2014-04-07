-- Реализуйте runBehaviour
-- (1 балл)

data Request  = Get           | Put String
data Response = Result String | OK

type Behaviour = [Response] -> [Request]

prog :: Behaviour
prog ~(OK : x : xs) = Put "more? " : Get : case x of
    Result "no" -> []
    Result "yes" -> prog xs
    _ -> Put "yes or no!" : prog (tail xs)

runBehaviour :: Behaviour -> IO ()
runBehaviour = runBehaviour' []

runBehaviour' :: [Response] -> Behaviour -> IO ()
runBehaviour' list p = 
    case drop (length list) (p list) of
        [] -> return ()
        x -> makeStep (head x)
    where
        makeStep Get = do
            s <- getLine
            runBehaviour' (list ++ [Result s]) p
        makeStep (Put s) = do
            putStrLn s
            runBehaviour' (list ++ [OK]) p
        

main = runBehaviour prog
