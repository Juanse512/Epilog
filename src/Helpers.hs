module Helpers where
import Data.Char
import Common


stringToInt :: String -> Int -> Int
stringToInt [] _ = 0
stringToInt (x:xs) pow = (digitToInt x) * (10^pow) + stringToInt xs (pow - 1)

lexNum :: String -> [EqToken]
lexNum [] = []
lexNum (' ':s) = lexNum s
lexNum ('+':s) = Plus:lexNum s
lexNum ('-':s) = Minus:lexNum s
lexNum (num:s) | isDigit num = let valStr = takeWhile (isDigit) (num:s)
                                   valInt = stringToInt valStr ((length valStr) - 1)
                               in (Num valInt):lexNum s
                | otherwise = let varStr = takeWhile (isAlpha) (num:s)
                              in (VarNum varStr):lexNum (dropWhile (isAlpha) (num:s))


evalNum :: [EqToken] -> Int -> Maybe Int
evalNum [] a = Just a
evalNum (Num i:xs) 0 = evalNum xs i
evalNum (Plus:Num i:xs) a = evalNum xs (a+i)
evalNum (Minus:Num i:xs) a = evalNum xs (a-i)
evalNum xs a = Nothing

parseAndEval :: String -> Maybe Int
parseAndEval str = evalNum (lexNum str) 0
-- parseNum s = case lexNum s of
--   Nothing -> []
--   Just (x,s') -> x:parseNum s'

-- evalNum (Num n:r) = evalNum (Plus:Num n:r)
-- evalNum str = evalNum' 0 str where
--   evalNum' acc (Plus:Num n:r) = evalNum' (acc+n) r
--   evalNum' acc (Minus:Num n:r) = evalNum' (acc-n) r