module Main
    ( main
    , boolP, maybeP, listP, listP', treeP
    ) where

import Combinators
import Test.HUnit
import Data.Char

-- (0.5 балла)
boolP :: Parser Bool
boolP = (const True <$> string "True") <|> (const False <$> string "False")

-- (0.5 балла)
maybeP :: Parser a -> Parser (Maybe a)
maybeP p = (const Nothing <$> string "Nothing") <|> 
           ((\_ x -> Just x) <$> string "Just " <*> p)

-- (0.5 балла)
listP :: Parser a -> Parser [a]
listP p = brackets $ sepBy p (char ',') 

-- (0.5 балла)
listP' :: Parser a -> Parser [a]
listP' p = brackets $ sepBy p (between spaces spaces (char ','))

data Tree a b = Node (Tree a b) a (Tree a b) | Leaf b deriving (Show, Eq)

-- (0.5 балла)
treeP :: Parser a -> Parser b -> Parser (Tree a b)
treeP pa pb = (Leaf <$> pb) <|>
              (angles $ Node <$> (treeP pa pb) <*> braces pa <*> (treeP pa pb))

main = fmap (const ()) $ runTestTT $ test
    $    label "pure"
    [ parserTest (pure 4) "qwerty" $ Just (4, "qwerty")
    , parserTest (pure 'x') "" $ Just ('x', "")
    ] ++ label "empty"
    [ parserTest (empty :: Parser ()) "qwerty" Nothing
    , parserTest (empty :: Parser ()) "" Nothing
    ] ++ label "satisfy"
    [ parserTest (satisfy (/= 'x')) "qwerty" $ Just ('q', "werty")
    , parserTest (satisfy (/= 'x')) "xwerty" Nothing
    ] ++ label "<*>"
    [ parserTest (max <$> satisfy (== 'q') <*> satisfy (/= 'x')) "qwerty" $ Just ('w', "erty")
    , parserTest ((+) <$> digit <*> digit) "5678" $ Just (11, "78")
    , parserTest (undefined <$> satisfy (== 'q') <*> satisfy (== 'x') :: Parser ()) "qwerty" Nothing
    , parserTest (undefined <$> satisfy (== 'x') <*> satisfy (== 'w') :: Parser ()) "qwerty" Nothing
    ] ++ label "<|>"
    [ parserTest (satisfy (== 'q') <|> satisfy (== 'x')) "qwerty" $ Just ('q', "werty")
    , parserTest (satisfy (== 'x') <|> satisfy (== 'q')) "qwerty" $ Just ('q', "werty")
    , parserTest (satisfy (== 'x') <|> satisfy (== 'y')) "qwerty" Nothing
    ] ++ label "eof"
    [ parserTest eof "qwerty" Nothing
    , parserTest eof "" $ Just ((), "")
    ] ++ label "char"
    [ parserTest (char 'q') "qwerty" $ Just ((), "werty")
    , parserTest (char 'x') "qwerty" Nothing
    ] ++ label "anyChar"
    [ parserTest anyChar "qwerty" $ Just ('q', "werty")
    , parserTest anyChar "" Nothing
    ] ++ label "digit"
    [ parserTest digit "qwerty" Nothing
    , parserTest digit "123qwerty" $ Just (1, "23qwerty")
    , parserTest digit "" Nothing
    ] ++ label "string"
    [ parserTest (string "qwerty") "qwerty" $ Just ((), "")
    , parserTest (string "qwerty") "qwertyuiop" $ Just ((), "uiop")
    , parserTest (string "qwerty") "qwerryuiop" Nothing
    , parserTest (string "qwerty") "qwert" Nothing
    ] ++ label "oneOf"
    [ parserTest (oneOf "xyz") "qwerty" Nothing
    , parserTest (oneOf "xyz") "xwerty" $ Just ('x', "werty")
    , parserTest (oneOf "xyz") "ywerty" $ Just ('y', "werty")
    , parserTest (oneOf "xyz") "zwerty" $ Just ('z', "werty")
    ] ++ label "many"
    [ parserTest (many (char 'q')) "qwerty" $ Just ([()], "werty")
    , parserTest (many (char 'q')) "qqqwerty" $ Just ([(),(),()], "werty")
    , parserTest (many (char 'q')) "werty" $ Just ([], "werty")
    , parserTest (many (char 'q')) "" $ Just ([], "")
    ] ++ label "many1"
    [ parserTest (many1 (char 'q')) "qwerty" $ Just ([()], "werty")
    , parserTest (many1 (char 'q')) "qqqwerty" $ Just ([(),(),()], "werty")
    , parserTest (many1 (char 'q')) "werty" Nothing
    , parserTest (many1 (char 'q')) "" Nothing
    ] ++ label "natural"
    [ parserTest natural "qwerty" Nothing
    , parserTest natural "123qwerty" $ Just (123, "qwerty")
    , parserTest natural "-123qwerty" Nothing
    , parserTest natural "" Nothing
    ] ++ label "integer"
    [ parserTest integer "qwerty" Nothing
    , parserTest integer "123qwerty" $ Just (123, "qwerty")
    , parserTest integer "-123qwerty" $ Just (-123, "qwerty")
    , parserTest integer "-qwerty" Nothing
    ] ++ label "spaces"
    [ parserTest spaces "qwerty" $ Just ((), "qwerty")
    , parserTest spaces "    qwerty" $ Just ((), "qwerty")
    , parserTest spaces "" $ Just ((), "")
    ] ++ label "try"
    [ parserTest (try natural) "123qwerty" $ Just (Just 123, "qwerty")
    , parserTest (try natural) "qwerty" $ Just (Nothing, "qwerty")
    , parserTest (try (char 'q')) "qwerty" $ Just (Just (), "werty")
    , parserTest (try (char 'x')) "qwerty" $ Just (Nothing, "qwerty")
    , parserTest (try (char 'x')) "" $ Just (Nothing, "")
    , parserTest (try eof) "qwerty" $ Just (Nothing, "qwerty")
    , parserTest (try eof) "" $ Just (Just (), "")
    ] ++ label "endBy"
    [ parserTest (natural `endBy` char ';') "1;2;3;456;xyz;" $ Just ([1,2,3,456], "xyz;")
    , parserTest (natural `endBy` char ';') "1;2;3;456" $ Just ([1,2,3], "456")
    , parserTest (natural `endBy` spaces) "12 25   300" $ Just ([12,25,300], "")
    , parserTest (natural `endBy` spaces) "qwerty" $ Just ([], "qwerty")
    , parserTest (natural `endBy` spaces) "" $ Just ([], "")
    ] ++ label "endBy1"
    [ parserTest (natural `endBy1` char ';') "1;2;3;456;xyz;" $ Just ([1,2,3,456], "xyz;")
    , parserTest (natural `endBy1` char ';') "1;2;3;456" $ Just ([1,2,3], "456")
    , parserTest (natural `endBy1` spaces) "12 25   300" $ Just ([12,25,300], "")
    , parserTest (natural `endBy1` spaces) "qwerty" Nothing
    , parserTest (natural `endBy1` spaces) "" Nothing
    ] ++ label "sepBy"
    [ parserTest (natural `sepBy` char ';') "1;2;3;456;xyz;" $ Just ([1,2,3,456], ";xyz;")
    , parserTest (natural `sepBy` char ';') "1;2;3;456" $ Just ([1,2,3,456], "")
    , parserTest (natural `sepBy` spaces) "12 25   300" $ Just ([12,25,300], "")
    , parserTest (natural `sepBy` spaces) "qwerty" $ Just ([], "qwerty")
    , parserTest (natural `sepBy` spaces) "" $ Just ([], "")
    ] ++ label "sepBy1"
    [ parserTest (natural `sepBy1` char ';') "1;2;3;456;xyz;" $ Just ([1,2,3,456], ";xyz;")
    , parserTest (natural `sepBy1` char ';') "1;2;3;456" $ Just ([1,2,3,456], "")
    , parserTest (natural `sepBy1` spaces) "12 25   300" $ Just ([12,25,300], "")
    , parserTest (natural `sepBy1` spaces) "qwerty" Nothing
    , parserTest (natural `sepBy1` spaces) "" Nothing
    ] ++ label "between"
    [ parserTest (between (char 'a') (char 'b') (char 'c')) "abc" Nothing
    , parserTest (between (char 'a') (char 'b') (char 'c')) "acb" $ Just ((), "")
    ] ++ label "brackets"
    [ parserTest (brackets (string "qwerty")) "[qwerty]uiop" $ Just ((), "uiop")
    , parserTest (brackets (string "qwerty")) "[qwertyu]iop" Nothing
    ] ++ label "parens"
    [ parserTest (parens spaces) "(   )qwerty" $ Just ((), "qwerty")
    , parserTest (parens spaces) "(q)werty" Nothing
    ] ++ label "braces"
    [ parserTest (braces natural) "{123}" $ Just (123, "")
    , parserTest (braces natural) "{}" Nothing
    ] ++ label "angles"
    [ parserTest (angles digit) "<1>" $ Just (1, "")
    , parserTest (angles digit) "<1 >" Nothing
    ] ++ label "boolP"
    [ parserTest boolP "Trueqwerty" $ Just (True, "qwerty")
    , parserTest boolP "False" $ Just (False, "")
    , parserTest boolP "qwerty" Nothing
    ] ++ label "maybeP"
    [ parserTest (maybeP natural) "Nothingqwerty" $ Just (Nothing, "qwerty")
    , parserTest (maybeP natural) "Just 123qwerty" $ Just (Just 123, "qwerty")
    , parserTest (maybeP natural) "Just123qwerty" Nothing
    ] ++ label "listP"
    [ evalParser (listP integer) "[1,-23,25,347]" ~?= Just [1,-23,25,347]
    , evalParser (listP integer) "[1 ,  -23,  25   ,347]" ~?= Nothing
    ] ++ label "listP'"
    [ evalParser (listP' integer) "[1,-23,25,347]" ~?= Just [1,-23,25,347]
    , evalParser (listP' integer) "[1 ,  -23,  25   ,347]" ~?= Just [1,-23,25,347]
    ] ++ label "treeP"
    [ evalParser (treeP integer integer) "100" ~?= Just (Leaf 100)
    , evalParser (treeP integer integer) "<1{2}3>" ~?= Just (Node (Leaf 1) 2 (Leaf 3))
    , evalParser (treeP integer integer) "<1{2}<3{4}5>>>" ~?= Just (Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5)))
    , evalParser (treeP integer integer) "<1{2}<3{4}5>" ~?= Nothing
    ] ++ label "foldl1P"
    [ evalParser (foldl1P (\a b c -> a ++ "[" ++ show b ++ "]" ++ c) (many $ satisfy $ not . isDigit) digit) "a1bcd5efg853h" ~?= Just "a[1]bcd[5]efg[8][5][3]h"
    , evalParser (foldl1P (\a b c -> "(" ++ a ++ [b] ++ c ++ ")") (fmap show natural) anyChar) "12+34+45+56" ~?= Just "(((12+34)+45)+56)"
    ] ++ label "foldr1P"
    [ evalParser (foldr1P (\a b c -> a ++ "[" ++ show b ++ "]" ++ c) (many $ satisfy $ not . isDigit) digit) "a1bcd5efg853h" ~?= Just "a[1]bcd[5]efg[8][5][3]h"
    , evalParser (foldr1P (\a b c -> "(" ++ a ++ [b] ++ c ++ ")") (fmap show natural) anyChar) "12+34+45+56" ~?= Just "(12+(34+(45+56)))"
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
