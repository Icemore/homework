import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr) 
import qualified Control.Exception as CE

main = do
    args <- getArgs
    case args of
        [] -> writeToStd
        _ -> printFiles args

printFiles :: [String] -> IO ()
printFiles [] = return ()
printFiles (x:xs) = do
    content <- CE.catch (readFile x) handle
    putStr content
    printFiles xs
        where handle e = let err = show (e :: CE.IOException) in
                do hPutStrLn stderr ("Couldn't open " ++ x ++ ": " ++ err); return ""

writeToStd :: IO ()
writeToStd = do
    s <- getLine
    putStrLn s
    writeToStd
