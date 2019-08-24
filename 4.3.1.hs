module Zzzzzzz where

import Data.List.Split
import Data.Char(isDigit)

g = "firstName=Barbarian\nlastName=Conn On\nage=30"

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving(Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving(Show)

sp :: String -> [String]
sp s = splitOn "\n" s

rmEmpty :: String -> String
rmEmpty s = filter (/=' ') s

rmSp :: String -> String
rmSp s = dropWhile (==' ') s

takeKey :: String -> String
takeKey s = rmEmpty $ takeWhile (/='=') s

takeValue :: String -> String
takeValue s = rmSp $ tail $ dropWhile (/='=') s

ff :: String -> Bool
ff s = case takeKey s of
	"firstName" -> True
	"lastName" -> True
	"age" -> True
	_ -> False

forAll :: String -> (Char -> Bool) -> Bool
forAll [] f= True
forAll s f = if f $ head s then forAll (tail s) f else False

forAllS :: [String] -> (String -> Bool) -> Bool
forAllS [] f = True
forAllS s f = if f $ head s then forAllS (tail s) f else False

parsePerson :: String -> Either Error Person
parsePerson s = if s == "" then Left ParsingError else validateAndProceed s

validateAndProceed :: String -> Either Error Person
validateAndProceed s = let 
	   parsed = sp s
	in if (forAllS parsed validate) then checkAllFields $ filter (\x -> ff x) parsed else Left ParsingError

validate :: String -> Bool
validate s =  let
	key = takeWhile (\x -> not (x == ' ')) (rmSp s)
	notKey  = dropWhile (\x -> not (x == ' ')) (rmSp s) 
	middle = if (length notKey > 3) then take 3 notKey else ""
	end = if (length notKey > 3) then drop 3 notKey else ""
  in case (key, middle, end) of 
  	("", _, _ ) -> False
  	(_, _, "") -> False
  	(s1, " = ", s2) -> True
  	_ -> False

checkAllFields :: [String] -> Either Error Person
checkAllFields m = if length m == 3 then checkAge m else Left IncompleteDataError

checkAge :: [String] -> Either Error Person
checkAge m = if forAll (takeValue x) isDigit then takeResult m else Left (IncorrectDataError (takeValue x)) where
	x = head (filter (\a -> (takeKey a) == "age") m)

takeResult :: [String] -> Either Error Person
takeResult m = Right (Person fn ln age) where
	fn = rmSp $ takeValue $ head m
	ln = rmSp $ takeValue $ head $ tail m
	age = (read $ takeValue $ last m)::Int

-- wrong Parse | empty string
t0 = parsePerson "" 

-- correct
t1 = parsePerson "firstName = John\nlastName = Connor\nage = 30"

-- correct | shiffled fields
t18 = parsePerson "lastName = Connor\nfirstName = John\nage = 30"

-- wrong Parse | no spaces around = in minor fields
t2 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"

 -- wrong Parse | no spaces around = on the left in minor fields
t5 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde= "

-- wrong Parse | no spaces around = in major fields
t3 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30"

 -- wrong Incorrect | age is non-numeric
t4 = parsePerson "firstName = John\nlastName = Connor\nage = as30"

-- wrong Parse | no spaces around = in major fields, missing major field
t6 = parsePerson "firstName=Barbarian\nlastName=Conn Or"

-- wrong Parse | no spaces around = in major fields, typo in major field
t7 = parsePerson "firstNameee = John Smith\nlastName = Connor\nage = 30\nasde=as11"

-- correct | excessive fields
t8 = parsePerson "firstName = John\nlastName = Connor\nfoo = bar\nage = 30"

-- wrong Incomplete | missing major field
t9 = parsePerson "firstName = Barbarian\nlastName = Conn Or"

-- wrong Parse | empty major value
t10 = parsePerson "firstName = John\nlastName = Connor\nage = "

-- wrong Parse | no spaces around = on the right in major field
t11 = parsePerson "firstName = John\nlastName = Connor\nage ="

-- wrong Parse | empty key, missing major field
t12 = parsePerson "firstName = John\nlastName = Connor\n = 30"

-- correct | spaces in major field value
t13 = parsePerson "firstName = Barbarian\nlastName = Conn On\nage = 30"

-- correct | = in major field value
t14 = parsePerson "firstName = John\nlastName = Con=nor\nage = 30"

-- wrong Parse | no spaces around =, missing value in minor field
t15 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd"

-- wrong Incomplete | major field key with whitespace, age is non-numeric
t17 = parsePerson " firstName = John\nlastName = Connor\nage = 2f8 "
