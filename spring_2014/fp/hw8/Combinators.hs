module Combinators
    ( module Parser
    , many, many1
    , char, anyChar, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

-- (0.5 балла)
char :: Char -> Parser ()
char ch = const () <$> satisfy (==ch)

-- (0.5 балла)
anyChar :: Parser Char
anyChar = satisfy $ const True

-- (0.5 балла)
digit :: Parser Int
digit = read . (:[]) <$> satisfy isDigit

-- (0.5 балла)
string :: String -> Parser ()
string (x:[]) = char x
string (x:xs) = const <$> char x <*> string xs

-- (0.5 балла)
oneOf :: String -> Parser Char
oneOf (x:[]) = satisfy (==x)
oneOf (x:xs)= satisfy (==x) <|> oneOf xs

-- (0.5 балла)
many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

-- (0.5 балла)
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- (0.5 балла)
natural :: Parser Integer
natural = foldl f 0 <$> map toInteger <$> many1 digit
    where f x y = x*10 + y

-- (0.5 балла)
integer :: Parser Integer
integer = ((\_ x -> -x) <$> char '-' <*> natural) <|> natural

-- (0.5 балла)
spaces :: Parser ()
spaces = const () <$> many (satisfy isSpace)

-- (0.5 балла)
try :: Parser a -> Parser (Maybe a)
try p = (Just <$> p) <|> pure Nothing

-- (0.5 балла)
endBy :: Parser a -> Parser b -> Parser [a]
endBy p1 p2 = endBy1 p1 p2 <|> pure []

-- (0.5 балла)
endBy1 :: Parser a -> Parser b -> Parser [a]
endBy1 p1 p2 = (\x _ xs -> x:xs) <$> p1 <*> p2 <*> endBy p1 p2

-- (0.5 балла)
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p1 p2 = sepBy1 p1 p2 <|> pure []

-- (0.5 балла)
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p1 p2 = ((\x _ xs -> x:xs) <$> p1 <*> p2 <*> sepBy1 p1 p2) <|> ((:[]) <$> p1)

-- (0.1 балла)
between :: Parser a -> Parser b -> Parser c -> Parser c
between a b c = (\_ x _ -> x) <$> a <*> c <*> b

-- (0.1 балла)
brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

-- (0.1 балла)
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- (0.1 балла)
braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

-- (0.1 балла)
angles :: Parser a -> Parser a
angles = between (char '<') (char '>')

-- (1 балл)
foldr1P :: (a -> b -> a -> a) -> Parser a -> Parser b -> Parser a
foldr1P f p1 p2 = (f <$> p1 <*> p2 <*> (foldr1P f p1 p2)) <|>
                  (f <$> p1 <*> p2 <*> p1) 

-- (1 балл)
foldl1P :: (a -> b -> a -> a) -> Parser a -> Parser b -> Parser a
foldl1P f p1 p2 = go <$> p1 <*> many1 ((,) <$> p2 <*> p1)
    where
        go = foldl (\x (b, a) -> f x b a)
