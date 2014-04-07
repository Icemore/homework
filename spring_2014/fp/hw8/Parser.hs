-- Список экспорта менять нельзя!
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , evalParser
    , parserTest
    ) where

import Control.Applicative
import Test.HUnit

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- (0.5 балла)
evalParser :: Parser a -> String -> Maybe a
evalParser p s = fst <$> runParser p s

-- (0.5 балла)
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser match
    where match s | s /= [] && f (head s) = Just (head s, tail s)
                  | otherwise = Nothing

-- (0.5 балла)
eof :: Parser ()
eof = Parser match
    where
        match [] = Just ((), [])
        match _ = Nothing

instance Functor Parser where
    fmap f (Parser p) = Parser match
        where 
            match s = case p s of
                Just (x, t) -> Just (f x, t)
                Nothing -> Nothing

instance Applicative Parser where
    -- (0.5 балла)
    pure x = Parser (\s -> Just (x, s))
    -- (1.5 балл)
    Parser f1 <*> Parser f2 = Parser match
        where 
            match s = case f1 s of
                Nothing -> Nothing
                Just (g, t) -> 
                    case f2 t of
                        Just (x, t2) -> Just (g x, t2)
                        Nothing -> Nothing


instance Alternative Parser where
    -- (0.5 балла)
    empty = Parser (\s -> Nothing)
    -- (0.5 балла)
    Parser p1 <|> Parser p2 = Parser (\s -> (p1 s) <|> (p2 s))

parserTest :: (Eq a, Show a) => Parser a -> String -> Maybe (a,String) -> Test
parserTest (Parser p) s e = p s ~?= e
