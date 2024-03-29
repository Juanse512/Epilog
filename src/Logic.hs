module Logic
  ( eval
  , Env
  )
where

import           Helpers
import           Common
import           Equations
import           VarMatch
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )
-- Entorno nulo
initEnv :: Env
initEnv = []

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure x = StateError $ \env -> Right (x :!: env)
  (<*>) = ap

instance Monad StateError where
  return = pure
  m >>= f = StateError $ \env -> case runStateError m env of
    Left e -> Left e
    Right (x :!: env') -> runStateError (f x) env'

instance MonadError StateError where
  throw e = StateError $ \_ -> Left e


instance MonadState StateError where
  lookfor v vars = StateError $ \env -> case functionSearch (v, vars) env True of
        Nothing -> Left UndefVar
        Just fun -> Right (fun :!: env)
  update v vars x = StateError $ \env -> Right (() :!: insertKey (v,vars) x env) 

-- eval: Dada una lista de expresiones, se evaluan partiendo de un entorno vacio 
eval :: [Exp] -> Either Error Exp
eval xs = eval' xs initEnv

-- eval': Evalua una lista de expresiones con un entorno dado
eval' :: [Exp] -> Env -> Either Error Exp
eval' [x] env = runStateError (stepComm x) env >>= (\e -> return (Data.Strict.Tuple.fst e))
eval' (x:Skip:xs) env = runStateError (stepComm x) env >>= (\e -> return (Data.Strict.Tuple.fst e))
eval' (x:xs) env = runStateError (stepComm x) env >>= (\e -> eval' xs (Data.Strict.Tuple.snd e))

-- stepComm: Evalua un Exp acorde a su función
stepComm :: (MonadState m, MonadError m) => Exp -> m Exp
stepComm Skip = return Skip

stepComm (ReturnVars vars) = return (ReturnVars vars)

stepComm (Fun name vars) = do
  v <- lookfor name vars
  res <- stepComm v 
  return res

stepComm (Assgn name vars exp) = do 
    update name vars exp
    return Skip
    
stepComm (Var (Equation e)) = case evalNum e of
                                Just res -> return (Var (Equation (Num res))) 
                                Nothing -> throw InvalidOp
     
stepComm (Var s) = return (Var s)
    
stepComm (RTrue) = return RTrue
    
stepComm (RFalse) = return RFalse

stepComm (Eq c1 c2) = do
    c1' <- stepComm c1 
    c2' <- stepComm c2
    if (c1' == c2') then return RTrue else return RFalse

stepComm (NEq c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    if (c1' /= c2') then return RTrue else return RFalse

stepComm (Or c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalLogic' c1' c2' (||)
    case res of 
      Left b -> return (boolToExp b)
      Right v -> return (ReturnVars v)

stepComm (And c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalLogic' c1' c2' (&&)
    case res of 
      Left b -> return (boolToExp b)
      Right v -> return (ReturnVars v)

stepComm (Add c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalNums c1' c2' (+)
    return (Var (Equation (Num res)))

stepComm (Sub c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalNums c1' c2' (-)
    return (Var (Equation (Num res)))

stepComm (Times c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalNums c1' c2' (*)
    return (Var (Equation (Num res)))

stepComm (Div c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalNums c1' c2' (div)
    return (Var (Equation (Num res)))


