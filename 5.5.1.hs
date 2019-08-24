module Zyyyyyyyqq where

main' = do
	putStrLn "What is your name?"
	putStr "Name: "
	name <- getLine
	if name == "" then main' else putStr ("Hi, " ++ name ++ "!")

