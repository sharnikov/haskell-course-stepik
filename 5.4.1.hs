module Zyyyyyyy where

import Data.Char
import Data.Maybe

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
    deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно



asToken :: String -> Maybe Token
asToken s = case s of
  "(" -> Just LeftBrace
  ")" -> Just RightBrace
  "+" -> Just Plus
  "-" -> Just Minus
  _   -> if (foldl (&&) True (map isDigit s)) then Just (Number (read s :: Int)) else Nothing

tokenize :: String -> Maybe [Token]
tokenize input = let 
     tokens = map (asToken) (words input)
     isEmpty = foldl (>>) (Just Plus) tokens
   in if isNothing isEmpty then Nothing else Just (map (fromJust) tokens)