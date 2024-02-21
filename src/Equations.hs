module Equations where
import Data.Char
import Common

-- stringToInt: Dado un string devuelve el número contenido
stringToInt :: String -> Int -> Int
stringToInt [] _ = 0
stringToInt (x:xs) pow = (digitToInt x) * (10^pow) + stringToInt xs (pow - 1)
-- lexNum: Dado un string con una ecuacion transforma el contenido a tokens de ecuacion
lexNum :: String -> EqToken
lexNum (' ':s) = lexNum s
lexNum (num:s) | isDigit num = let valStr = takeWhile (isDigit) (num:s)
                                   valInt = stringToInt valStr ((length valStr) - 1)
                               in (Num valInt)
                | otherwise = let varStr = takeWhile (isAlpha) (num:s)
                              in VarNum varStr

-- evalNum: Evalua una ecuación y devuelve el valor
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

