import Data.List

main = do
    s <- readFile "input"
    writeFile "output" $ toStr $ transpose $ parseMat s

parseMat :: String -> [[Int]]
parseMat str = map go (lines str)
    where go line = map read (words line)
 
toStr :: [[Int]] -> String
toStr mat = unlines $ map unwords go
    where go = map (map show) mat
