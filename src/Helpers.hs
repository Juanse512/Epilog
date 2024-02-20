module Helpers where
import Data.Char
import Common


stringToInt :: String -> Int -> Int
stringToInt [] _ = 0
stringToInt (x:xs) pow = (digitToInt x) * (10^pow) + stringToInt xs (pow - 1)

lexNum :: String -> EqToken
lexNum (' ':s) = lexNum s
lexNum (num:s) | isDigit num = let valStr = takeWhile (isDigit) (num:s)
                                   valInt = stringToInt valStr ((length valStr) - 1)
                               in (Num valInt)
                | otherwise = let varStr = takeWhile (isAlpha) (num:s)
                              in (VarNum varStr)


evalNum :: EqToken -> Maybe Int
evalNum (Num i) = return i
evalNum (VarNum i) = Nothing
evalNum (Plus a b) = do a' <- evalNum a
                        b' <- evalNum b
                        return (a' + b')
evalNum (Minus a b) = do a' <- evalNum a
                         b' <- evalNum b
                         return (a' - b')
evalNum (NumTimes a b) = do a' <- evalNum a
                            b' <- evalNum b
                            return (a' * b')
evalNum (NumDiv a b) =   do a' <- evalNum a
                            b' <- evalNum b
                            return (a' `div` b')

parseAndEval :: String -> Maybe Int
parseAndEval str = evalNum (lexNum str)
