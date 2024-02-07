module Logic
  ( eval
  , Env
  )
where

import           Common
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
-- data Name = Generic String
          -- | Value String
          -- | Function String
          -- | ReturnValue Bool
varMatcher :: [VarT] -> [VarT] -> Bool
varMatcher [] [] = True
varMatcher [] xs = False
varMatcher xs [] = False
varMatcher ((VarN (Value s)):xs) ((VarN (Value ss)):xxs) = if s == ss then varMatcher xs xxs else False
varMatcher _ _ = False

genericVarMatcher :: [VarT] -> [VarT] -> Bool
genericVarMatcher [] [] = True
genericVarMatcher [] xs = False
genericVarMatcher xs [] = False
genericVarMatcher ((VarN (Value s)):xs) ((VarN (Generic ss)):xss) = varMatcher xs xss
genericVarMatcher ((VarN (Generic s)):xs) ((VarN (Generic ss)):xss) = varMatcher xs xss
genericVarMatcher ((VarN (Value s)):xs) ((VarN (Value ss)):xxs) = if s == ss then varMatcher xs xxs else False

functionMatcher :: (Name, [VarT]) -> [((Name, [VarT]), Exp)] -> ([Exp], [Exp])
functionMatcher (name, vars) [] = ([], [])
functionMatcher (name, vars) (((fname, fvars), exp):xs) | name == fname =  let varMatch = varMatcher vars fvars
                                                                               genericMatch = genericVarMatcher vars fvars
                                                                               (left, right) = functionMatcher (name, vars) xs
                                                                            in if varMatch then (exp:left, right)
                                                                                else if genericMatch then (left, exp:right)
                                                                                    else (left, right)
                                                        | otherwise = functionMatcher (name, vars) xs

searchResult :: [Exp] -> Maybe Exp
searchResult [] = Nothing
searchResult ((RTrue):xs) = Just RTrue
searchResult ((RFalse):xs) = Just RFalse
searchResult (x:xs) = searchResult xs


functionSearch :: (Name, [VarT]) -> [((Name, [VarT]), Exp)] -> Maybe Exp
functionSearch search envlist = let (match, generic) = functionMatcher search envlist
                                in case (searchResult (match ++ generic)) of
                                        Just x -> Just x
                                        Nothing -> case match of 
                                                    [] -> case generic of 
                                                            [] -> Nothing
                                                            xs -> Just (head xs)
                                                    xs -> Just (head xs)



instance Ord Name where
    compare (Generic _) (Value _) = GT
    compare (Generic _) (Function _) = GT
    compare (Generic _) (ReturnValue _) = GT
    compare (Value _) (Function _) = GT
    compare (Value _) (ReturnValue _) = GT
    compare (Function _) (ReturnValue _) = GT
    compare (Generic n) (Generic n2) = compare n n2
    compare (Value n) (Value n2) = compare n n2
    compare (Function n) (Function n2) = compare n n2
    compare (ReturnValue n) (ReturnValue n2) = compare n n2
    
data Key = K (Name,[VarT]) deriving (Show, Eq)
instance Ord Key where
    compare (K (n1, v1)) (K (n2, v2)) = if v1 /= v2 then compare n1 n2 else LT
-- instance Eq Key where
--     (K (n1, vars1)) == (K (n2, vars2)) = (n1 == n2) && (vars1 == vars2)
type Env = M.Map Key Exp

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError $ \env -> Right (x :!: env)
  m >>= f = StateError $ \env -> case runStateError m env of
    Left e -> Left e
    Right (x :!: env') -> runStateError (f x) env'

instance MonadError StateError where
  throw e = StateError $ \_ -> Left e

-- Modificar funcion de busqueda
-- Logica de busqueda:
-- 1) Si hay returnValue devolver ese
-- 2) Si no, buscar matches con values
-- 3) Si no, buscar matches con generic y luego reemplazar las variables
-- 4) Si hay mas de un match, ver que hace prolog y copiarlo
keyToList :: [(Key, Exp)] -> [((Name, [VarT]), Exp)]
keyToList [] = []
keyToList ((K p, exp):xs) = (p, exp):(keyToList xs)


instance MonadState StateError where
  --falta logica para matchear variables genericas y valores
  -- lookfor v vars = StateError $ \env -> case functionSearch (v, vars) (keyToList (M.toList env)) of
  lookfor v vars = StateError $ \env -> case M.lookup (K (v, vars)) env of
        Nothing -> Left UndefVar
        Just fun -> Right (fun :!: env)
  update v vars x = StateError $ \env -> Right (() :!: M.insert (K (v,vars)) x env)
-- HAY ERROR CON LAS VARIABLES, NO DIFERENCIA ENTRE MISMA FUNCION DISTINTAS VARIABLES
-- ESTO SE ARREGLA HACIENDO LA FUNCION DE BUSQUEDA BIEN CREO
-- eval :: [Exp] -> Either Error Exp
-- eval comm = case runStateError (stepCommStar comm) initEnv of
--   Left e -> Left e
--   Right (exp :!: env) -> Right exp

eval :: [Exp] -> Either Error Exp
eval xs = eval' xs initEnv

-- eval' :: [Exp] -> Env -> Either Error Exp
-- eval' [x] env = runStateError (stepCommStar x) env >>= (\e -> return Skip)
-- eval' (x:xs) env = runStateError (stepCommStar x) env >>= (\e -> eval' xs (Data.Strict.Tuple.snd e))
eval' :: [Exp] -> Env -> Either Error Exp
eval' [x] env = runStateError (stepComm x) env >>= (\e -> return (Data.Strict.Tuple.fst e))
eval' (x:Skip:xs) env = runStateError (stepComm x) env >>= (\e -> return (Data.Strict.Tuple.fst e))
eval' (x:xs) env = runStateError (stepComm x) env >>= (\e -> eval' xs (Data.Strict.Tuple.snd e))


stepComm :: (MonadState m, MonadError m) => Exp -> m Exp
stepComm Skip = return Skip

stepComm (Fun name vars) = do
  v <- evalFun name vars
  res <- stepComm v 
  return res

stepComm (Assgn name vars exp) = do 
    update name vars exp
    return Skip
    
stepComm (Var s) = return (Var s)
    
    

stepComm (RTrue) = return RTrue
    
    

stepComm (RFalse) = return RFalse
-- stepComm (Seq c1 c2) = do
--     c1' <- eval c1
--     c2' <- eval c2
--     return c2'


-- Esta bien esto? No tengo que evaluar c2?
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do
  c1' <- stepComm c1
  return (Seq c1' c2)

stepComm (Eq c1 c2) = do
    c1' <- stepComm c1 
    c2' <- stepComm c2
    -- return Right (c1' == c2' :!: env)
    return (Var (VarN (ReturnValue (c1' == c2'))))

stepComm (NEq c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    return (Var (VarN (ReturnValue (c1' /= c2'))))

-- stepComm (Or c1 c2) = do
--     c1' <- eval c1
--     c2' <- eval c2
--     res <- evalLogic c1' c2' (||)
--     return res

-- stepComm (And c1 c2) = do
--     c1' <- eval c1
--     c2' <- eval c2
--     res <- evalLogic c1' c2' (&&)
--     return res

evalFun name vars = lookfor name vars

evalLogic (Var (VarN (ReturnValue c1))) (Var (VarN (ReturnValue c2))) op = return (op c1 c2)
evalLogic _ _ _ = throw InvalidOp 

-- stepComm (IfThenElse b c1 c2) = do
--   b' <- evalExp b
--   if b' then return c1 else return c2

-- stepComm r@(While bexp c) = return $ IfThenElse bexp r Skip
