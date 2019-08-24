module Ppp where 

import Data.Char

d :: String -> String
d x = unwords.filter (any isLower).words $ x
