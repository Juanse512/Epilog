module Helpers where
import Common
import Monads
import Equations

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
expToBool :: MonadError m => Exp -> m Bool
expToBool RTrue = return True
expToBool RFalse = return False
expToBool _ = throw InvalidOp

-- evalLogic: Dados dos Exps booleanos y una operación, da esa operación y la devuelve como bool
evalLogic :: MonadError m => Exp -> Exp -> (Bool -> Bool -> Bool) -> m Bool
evalLogic a b op = do a' <- expToBool a  
                      b' <- expToBool b
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
