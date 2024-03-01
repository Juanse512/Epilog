module Helpers where
import Common
import Monads
import Equations
import qualified Data.Set as S

getVars :: Key -> [VarT]
getVars (name, vars) = vars

getName :: Key -> VarT
getName (name, vars) = name

isFun :: Exp -> Bool
isFun (Fun _ _) = True
isFun _ = False

splitFun :: Exp -> Key
splitFun (Fun name vars) = (name, vars)

-- insertKey: Dada una key y un valor inserta el nuevo elemento en el entorno
insertKey :: Key -> Exp -> Env -> Env
insertKey k exp [] = [(k, exp)]
insertKey k exp ((k2, exp2):xs) = if k == k2 then (k, exp):xs else (k2, exp2):(insertKey k exp xs)

-- expToBool: Transfora Exp booleanos a Bool 
expToBool :: Exp -> Bool
expToBool RTrue = True
expToBool RFalse = False

-- isBool: Determina si el Exp representa un Bool
isBool :: Exp -> Bool
isBool RTrue = True
isBool RFalse = True
isBool _ = False

-- getReturnVars: Devuelve las variables de retorno de un tipo ReturnVars
getReturnVars :: MonadError m => Exp -> m [[VarT]]
getReturnVars (ReturnVars a) = return a
generateVars _ = throw InvalidOp

-- isGeneric: Determina si la variable es generica
isGeneric :: VarT -> Bool
isGeneric (Generic _) = True
isGeneric _ = False

-- isSubsetOf: Determina si un conjunto es un subconjunto de otro 
isSubsetOf these those =
    case these of
        [] -> True
        c : cs -> (c `elem` those) && (isSubsetOf cs those)

-- cmpReturnVars': Dado una lista de variables y una lista de variables de retorno
-- verifica si los valores de las variables genericas coinciden.
cmpReturnVars' :: [VarT] -> [[VarT]] -> Bool
cmpReturnVars' xs [] = True
cmpReturnVars' (x:xs) (y:ys) = if x == (head y) then ((isSubsetOf (x:xs) y) || (isSubsetOf y (x:xs))) && cmpReturnVars' (x:xs) ys
                               else cmpReturnVars' (x:xs) ys
-- cmpReturnVars: Dada dos listas de variables de retorno verifica que sus posibles valores coincidan
cmpReturnVars :: [[VarT]] -> [[VarT]] -> Bool
cmpReturnVars [] ys = True
cmpReturnVars (x:xs) ys = if isGeneric (head x) then (cmpReturnVars' x ys) && (cmpReturnVars xs ys)
                          else (cmpReturnVars xs ys)
-- evalLogic': Dados dos Exps y una operación hace la siguiente verificiación:
-- Si ambos son booleanos, los compara
-- Si es una instruccion con variables genericas compara las variables que retornan y devuelve True si
-- coinciden y False si no. En el caso de que haya dos tipos de variable, retorna el conjunto
-- de variables de retorno para seguir la comparación en otra operación. 
evalLogic' :: MonadError m => Exp -> Exp -> (Bool -> Bool -> Bool) -> m (Either Bool [[VarT]])
evalLogic' a b op = if ((isBool a) && (isBool b)) then do res <- evalLogic a b op
                                                          return (Left res)
                    else if isBool a 
                         then if not (op (expToBool a) True) then return (Left False)
                              else (do v <- getReturnVars b
                                       return (Right v))
                         else if isBool b 
                              then if not (op (expToBool b) True) then return (Left False)
                                   else (do v <- getReturnVars a
                                            return (Right v))
                              else do v1 <- (getReturnVars a)
                                      v2 <- (getReturnVars b)
                                      return (Left (cmpReturnVars v1 v2))
                                


-- evalLogic: Dados dos Exps booleanos y una operación, da esa operación y la devuelve como bool
evalLogic :: MonadError m => Exp -> Exp -> (Bool -> Bool -> Bool) -> m Bool
evalLogic a b op = do let a' = expToBool a  
                      let b' = expToBool b
                      return (op a' b')
                        
                                       
                                

-- evalNums: Dados dos Exps aritmeticos y una operación, da esa operación y la devuelve como Int
evalNums :: MonadError m => Exp -> Exp -> (Int -> Int -> Int) -> m Int
evalNums (Var (Equation a)) (Var (Equation b)) op = let e1 = evalNum a
                                                        e2 = evalNum b
                                                    in case e1 of
                                                          Nothing -> throw InvalidOp
                                                          Just e1' -> case e2 of
                                                                      Nothing -> throw InvalidOp
                                                                      Just e2' -> return (op e1' e2')
evalNums a b op = throw InvalidOp

-- boolToExp: Transfora un Bool a su correspondiente valor en Exp
boolToExp :: Bool -> Exp
boolToExp True = RTrue
boolToExp False = RFalse