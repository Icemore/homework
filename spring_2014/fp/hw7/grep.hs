import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr) 
import qualified Control.Exception as CE
import Data.List (isInfixOf)
import Data.Foldable (forM_)

main = do
    args <- getArgs
    case args of
        (pattern:files) -> findInFiles pattern files
        [] -> hPutStrLn stderr "Wrong number of arguments"

findInFiles :: String -> [String] -> IO ()
findInFiles pattern (x:xs) = do 
    findInFile pattern x
    findInFiles pattern xs
findInFiles _ [] = return ()

findInFile :: String -> String -> IO ()
findInFile pattern fileName = do
    content <- CE.catch (readFile fileName) handle
    forM_ (lines content) findInLine
    where
        findInLine str = if isInfixOf pattern str 
                         then putStrLn (fileName ++ ":" ++ str)
                         else return ()
        handle e = let err = show (e :: CE.IOException) in
                do hPutStrLn stderr ("Couldn't open " ++ fileName ++ ": " ++ err); return ""
