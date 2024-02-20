module Logic
  ( eval
  , Env
  )
where

import           Common
import           Helpers
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.List
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )


varMatcher :: [VarT] -> [VarT] -> Bool
varMatcher [] [] = True
varMatcher [] xs = False
varMatcher xs [] = False
varMatcher ((VarN (Value s)):xs) ((VarN (Value ss)):xxs) = if s == ss then varMatcher xs xxs else False
varMatcher ((VarN (Joker _)):xs) ((VarN (Value ss)):xxs) = varMatcher xs xxs
varMatcher _ _ = False
genericVarMatcher :: [VarT] -> [VarT] -> Bool
genericVarMatcher [] [] = True
genericVarMatcher [] xs = False
genericVarMatcher xs [] = False
genericVarMatcher ((VarN (Value s)):xs) ((VarN (Generic ss)):xss) = genericVarMatcher xs xss
genericVarMatcher ((VarN (Generic s)):xs) ((VarN (Generic ss)):xss) = genericVarMatcher xs xss
genericVarMatcher ((VarN (Equation s)):xs) ((VarN (Generic ss)):xss) = genericVarMatcher xs xss
genericVarMatcher ((VarN (Equation s)):xs) ((VarN (Equation ss)):xss) = if s == ss then genericVarMatcher xs xss else False
genericVarMatcher ((VarN (Generic s)):xs) ((VarN (Equation ss)):xss) = genericVarMatcher xs xss 
genericVarMatcher ((VarN (Value s)):xs) ((VarN (Value ss)):xss) = if s == ss then genericVarMatcher xs xss else False
genericVarMatcher ((VarN (List s)):xs) ((VarN (List ss)):xss) = if s == ss then genericVarMatcher xs xss else False
genericVarMatcher ((VarN (List [])):xs) ((VarN (HeadTail _ _)):xss) = False
genericVarMatcher ((VarN (List s)):xs) ((VarN (HeadTail _ _)):xss) = genericVarMatcher xs xss
genericVarMatcher ((VarN (Joker s)):xs) (x:xss) = False

functionMatcher :: (Name, [VarT]) -> [((Name, [VarT]), Exp)] -> ([(Exp, [VarT])], [(Exp, [VarT])])
functionMatcher (name, vars) [] = ([], [])
functionMatcher (name, vars) (((fname, fvars), exp):xs) | name == fname =  let varMatch = varMatcher vars fvars
                                                                               genericMatch = genericVarMatcher vars fvars
                                                                               (left, right) = functionMatcher (name, vars) xs
                                                                            in if varMatch then ((exp, fvars):left, right)
                                                                                else if genericMatch then (left, (exp, fvars):right)
                                                                                    else (left, right)
                                                        | otherwise = functionMatcher (name, vars) xs

searchResult :: [Exp] -> Maybe Exp
searchResult [] = Nothing
searchResult ((RTrue):xs) = Just RTrue
searchResult ((RFalse):xs) = Just RFalse
searchResult (x:xs) = searchResult xs

pairGenericVars :: [VarT] -> [VarT] -> [(VarT, VarT)]
pairGenericVars [] xss = []
pairGenericVars xs [] = []
pairGenericVars ((VarN (Value s)):xs) ((VarN (Value y)):xss) = pairGenericVars xs xss
pairGenericVars ((VarN (Value s)):xs) ((VarN (Generic y)):xss) = ((VarN (Value s), VarN (Generic y))):pairGenericVars xs xss
pairGenericVars ((VarN (Equation s)):xs) ((VarN (Generic y)):xss) = ((VarN (Equation s), VarN (Generic y))):pairGenericVars xs xss
pairGenericVars ((VarN (List s)):xss) ((VarN (HeadTail x xs)):xsss) = (head s, VarN x):((VarN (List (tail s))), VarN xs):pairGenericVars xss xsss
pairGenericVars (x:xs) (y:ys) = pairGenericVars xs ys

compareVars :: VarT -> [(VarT, VarT)] -> VarT
compareVars var [] = var
compareVars var ((value, generic):xs) = if var == generic then value else compareVars var xs
compareEquationVars :: EqToken -> [(VarT, VarT)] -> EqToken
compareEquationVars a [] = a
compareEquationVars (VarNum s) (((VarN (Equation e)), (VarN (Generic g))):xs) | s == g = case evalNum e 0 of 
                                                                                            Just ev -> (Num ev)
                                                                                            Nothing -> (VarNum s)
                                                                              | otherwise = compareEquationVars (VarNum s) xs
compareEquationVars a (x:xs) = compareEquationVars a xs

searchForVarsInEq :: [EqToken] -> [(VarT, VarT)] -> [EqToken]
searchForVarsInEq [] map = []
searchForVarsInEq ((VarNum x):xs) map = (compareEquationVars (VarNum x) map):searchForVarsInEq xs map
searchForVarsInEq (x:xs) map = x:searchForVarsInEq xs map

replaceGenericVars :: [VarT] -> [(VarT, VarT)] -> Maybe [VarT]
replaceGenericVars [] xs = return []
replaceGenericVars ((VarN (Generic s)):xs) xss = do rest <- replaceGenericVars xs xss
                                                    return ((compareVars (VarN (Generic s)) xss):rest)
replaceGenericVars ((VarN (Equation eq)):xs) xss = do rest <- replaceGenericVars xs xss 
                                                      let eqT = searchForVarsInEq eq xss
                                                      evalEq <- evalNum eqT 0
                                                      return ((VarN (Equation ([Num evalEq]))):rest)
replaceGenericVars (x:xs) xss = do rest <- replaceGenericVars xs xss
                                   return (x:rest)

replaceFunctionVars :: [(VarT, VarT)] -> Exp -> Maybe Exp
replaceFunctionVars vars (Fun name fvars) = do fvars <- (replaceGenericVars fvars vars)
                                               return (Fun name fvars)
replaceFunctionVars vars (Seq a b) = do fa <- (replaceFunctionVars vars a)
                                        fb <- (replaceFunctionVars vars b)
                                        return (Seq fa fb)  
replaceFunctionVars vars (NEq a b) = do fa <- (replaceFunctionVars vars a)
                                        fb <- (replaceFunctionVars vars b)
                                        return (NEq fa fb)  
replaceFunctionVars vars (Eq a b) = do fa <- (replaceFunctionVars vars a)
                                       fb <- (replaceFunctionVars vars b)
                                       return (Eq fa fb)  
replaceFunctionVars vars (And a b) = do fa <- (replaceFunctionVars vars a)
                                        fb <- (replaceFunctionVars vars b)
                                        return (And fa fb)  
replaceFunctionVars vars (Or a b) = do fa <- (replaceFunctionVars vars a)
                                       fb <- (replaceFunctionVars vars b)
                                       return (Or fa fb)  
replaceFunctionVars vars (Add a b) = do fa <- (replaceFunctionVars vars a)
                                        fb <- (replaceFunctionVars vars b)
                                        return (Add fa fb)  
replaceFunctionVars vars (Sub a b) = do fa <- (replaceFunctionVars vars a)
                                        fb <- (replaceFunctionVars vars b)
                                        return (Sub fa fb)  
replaceFunctionVars vars exp = return exp





getVars :: Key -> [VarT]
getVars (name, vars) = vars

getName :: Key -> Name
getName (name, vars) = name

isFun :: Exp -> Bool
isFun (Fun _ _) = True
isFun _ = False

splitFun :: Exp -> Key
splitFun (Fun name vars) = (name, vars)


searchForMatch:: Key -> [(Key, Exp)] -> Bool -> Maybe Exp
searchForMatch search envlist depth = let (match, generic) = functionMatcher search envlist
                                      in case (searchResult ((map Prelude.fst match) ++ (map Prelude.fst generic))) of
                                              Just x -> Just x
                                              Nothing -> case match of 
                                                          [] -> case generic of 
                                                                  [] -> Nothing
                                                                  xs -> let (exp, vars) = (head xs)
                                                                            varsPair = pairGenericVars (getVars search) vars
                                                                            searchResultM = (replaceFunctionVars varsPair exp)
                                                                        in case searchResultM of
                                                                            Just searchResult ->  if not depth then Just searchResult
                                                                                                  else  if not (isFun searchResult) 
                                                                                                             then Just searchResult
                                                                                                        else let resKey = splitFun searchResult
                                                                                                                 dupSearch = searchForMatch resKey envlist False
                                                                                                              in case dupSearch of
                                                                                                                    Just x -> if x == (Fun (getName search) (getVars search))
                                                                                                                              then Nothing
                                                                                                                              else Just searchResult
                                                                                                                    Nothing -> Just searchResult
                                                                            Nothing -> Nothing
                                                                                  
                                                          xs -> Just (Prelude.fst (head xs))

prepareVarsForGenericSearch :: [VarT] -> [VarT]
prepareVarsForGenericSearch [] = []
prepareVarsForGenericSearch (VarN (Generic a):xs) = (VarN (Joker a)):prepareVarsForGenericSearch xs 
prepareVarsForGenericSearch (x:xs) = x:prepareVarsForGenericSearch xs 
-- Solamente devuelvo los que equivalen a RTrue, tambien tengo que ver que me traiga las variables
searchForGenerics:: Key -> [(Key, Exp)] -> Bool -> Maybe Exp
searchForGenerics search envlist depth = let  vars = prepareVarsForGenericSearch (getVars search)
                                              name = getName search
                                              (matches, _) = functionMatcher (name, vars) envlist
                                              varArr = map Prelude.snd matches
                                         in case matches of
                                              [] -> Nothing
                                              xs -> Just (ReturnVars (transpose ((getVars search):varArr)))

searchType :: [VarT] -> Bool
searchType [] = True
searchType ((VarN (Generic _)):xs) = False
searchType (x:xs) = searchType xs

functionSearch :: Key -> [(Key, Exp)] -> Bool -> Maybe Exp
functionSearch search envlist depth = if searchType (getVars search) then
                                         searchForMatch search envlist depth  
                                      else searchForGenerics search envlist depth

insertKey :: Key -> Exp -> Env -> Env
insertKey k exp [] = [(k, exp)]
insertKey k exp ((k2, exp2):xs) = if k == k2 then (k, exp):xs else (k2, exp2):(insertKey k exp xs)


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
    
type Key = (Name, [VarT])
type Env = [(Key, Exp)]

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
  pure  = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError $ \env -> Right (x :!: env)
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
  v <- evalFun name vars
  res <- stepComm v 
  return res

stepComm (Assgn name vars exp) = do 
    update name vars exp
    return Skip
    
stepComm (Var s) = return (Var s)
    
    

stepComm (RTrue) = return RTrue
    
    

stepComm (RFalse) = return RFalse



-- Esta bien esto? No tengo que evaluar c2?
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do
  c1' <- stepComm c1
  return (Seq c1' c2)

stepComm (Eq c1 c2) = do
    c1' <- stepComm c1 
    c2' <- stepComm c2
    return (Var (VarN (ReturnValue (c1' == c2'))))

stepComm (NEq c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    return (Var (VarN (ReturnValue (c1' /= c2'))))

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
    return (Var (VarN (Equation [Num res])))

stepComm (Sub c1 c2) = do
    c1' <- stepComm c1
    c2' <- stepComm c2
    res <- evalNums c1' c2' (-)
    return (Var (VarN (Equation [Num res])))


evalFun name vars = lookfor name vars

evalLogic (Var (VarN (ReturnValue c1))) (Var (VarN (ReturnValue c2))) op = return (op c1 c2)
evalLogic RTrue RTrue op = return True
evalLogic RTrue RFalse (&&) = return False
evalLogic RFalse RTrue (&&) = return False
evalLogic RFalse RTrue (||) = return True
evalLogic RTrue RFalse (||) = return True
evalLogic RFalse RFalse op = return False
evalLogic _ _ _ = throw InvalidOp 

evalNums (Var (VarN (Equation a))) (Var (VarN (Equation b))) op = let e1 = evalNum a 0
                                                                      e2 = evalNum b 0
                                                                  in case e1 of
                                                                       Nothing -> throw InvalidOp
                                                                       Just e1' -> case e2 of
                                                                                    Nothing -> throw InvalidOp
                                                                                    Just e2' -> return (op e1' e2')
evalNums a b op = throw InvalidOp


