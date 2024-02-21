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
                                          
eval :: [Exp] -> Either Error Exp
eval xs = eval' xs initEnv

eval' :: [Exp] -> Env -> Either Error Exp
eval' [x] env = runStateError (stepComm x) env >>= (\e -> return (Data.Strict.Tuple.fst e))
eval' (x:Skip:xs) env = runStateError (stepComm x) env >>= (\e -> return (Data.Strict.Tuple.fst e))
eval' (x:xs) env = runStateError (stepComm x) env >>= (\e -> eval' xs (Data.Strict.Tuple.snd e))

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
    
stepComm (Var s) = return (Var s)
    
stepComm (RTrue) = return RTrue
    
stepComm (RFalse) = return RFalse

stepComm (Eq c1 c2) = do
    c1' <- stepComm c1 
    c2' <- stepComm c2
    return (Var (ReturnValue (c1' == c2')))

stepComm (NEq c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    return (Var (ReturnValue (c1' /= c2')))

stepComm (Or c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalLogic c1' c2' (||)
    if res then return RTrue else return RFalse

stepComm (And c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalLogic c1' c2' (&&)
    if res then return RTrue else return RFalse

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


