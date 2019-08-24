module Zyyyyyyyqqt where

import Control.Monad.Reader

type User = String
type Password = String
type UsersTable = [(User, Password)]


usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
	l <- ask
	return $ map fst (filter (\(log, pass) -> pass == "123456") l)


type Shopping = Writer Integer String

purchase :: String -> Integer -> Shopping
purchase item cost = do
    tell "item"
    return cost

total :: Shopping -> Integer
total m = fst (runWriter m) 

items :: Shopping -> [String]
items m = snd (runWriter m)