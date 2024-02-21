module VarMatch where
import Common
import Equations
import Helpers
import Data.List
-- varMatcher: Compara dos listas de variables y retorna si coinciden completamente
-- se utiliza el Joker en el caso de la busqueda generica
varMatcher :: [VarT] -> [VarT] -> Bool
varMatcher [] [] = True
varMatcher [] xs = False
varMatcher xs [] = False
varMatcher ((Value s):xs) ((Value ss):xxs) = if s == ss then varMatcher xs xxs else False
varMatcher ((Joker _):xs) ((Value ss):xxs) = varMatcher xs xxs
varMatcher _ _ = False
-- genericVarMatcher: Compara dos listas de variables y retorna si coinciden
-- con el criterio de las variables genericas, es decir, si hay una variable generica
-- o una ecuacion se permite devolver True
genericVarMatcher :: [VarT] -> [VarT] -> Bool
genericVarMatcher [] [] = True
genericVarMatcher [] xs = False
genericVarMatcher xs [] = False
genericVarMatcher ((Value s):xs) ((Generic ss):xss) = genericVarMatcher xs xss
genericVarMatcher ((Generic s):xs) ((Generic ss):xss) = genericVarMatcher xs xss
genericVarMatcher ((Equation s):xs) ((Generic ss):xss) = genericVarMatcher xs xss
genericVarMatcher ((Equation s):xs) ((Equation ss):xss) = if s == ss then genericVarMatcher xs xss else False
genericVarMatcher ((Generic s):xs) ((Equation ss):xss) = genericVarMatcher xs xss 
genericVarMatcher ((Value s):xs) ((Value ss):xss) = if s == ss then genericVarMatcher xs xss else False
genericVarMatcher ((List s):xs) ((List ss):xss) = if s == ss then genericVarMatcher xs xss else False
genericVarMatcher ((List []):xs) ((HeadTail _ _):xss) = False
genericVarMatcher ((List s):xs) ((HeadTail _ _):xss) = genericVarMatcher xs xss
genericVarMatcher ((Joker s):xs) (x:xss) = False
genericVarMatcher ((Value s):xs) ((Equation ss):xss) = False

-- functionMatcher: Dada una key y un entorno, devuelve un par de arrays, el primero
-- contiene los matches perfectos y las variables que contiene, y segundo
-- los matches genericos con sus variables
functionMatcher :: Key -> [(Key, Exp)] -> ([(Exp, [VarT])], [(Exp, [VarT])])
functionMatcher (name, vars) [] = ([], [])
functionMatcher (name, vars) (((fname, fvars), exp):xs) | name == fname =  let varMatch = varMatcher vars fvars
                                                                               genericMatch = genericVarMatcher vars fvars
                                                                               (left, right) = functionMatcher (name, vars) xs
                                                                            in if varMatch then ((exp, fvars):left, right)
                                                                                else if genericMatch then (left, (exp, fvars):right)
                                                                                    else (left, right)
                                                        | otherwise = functionMatcher (name, vars) xs

-- searchResult: Detecta si hay alguna respuesta booleana en los resultados de la busqueda
searchResult :: [Exp] -> Maybe Exp
searchResult [] = Nothing
searchResult ((RTrue):xs) = Just RTrue
searchResult ((RFalse):xs) = Just RFalse
searchResult (x:xs) = searchResult xs

-- pairGenericVars: Dadas las variables de la busqueda y las variables del resultado de esta
-- devuelve un par que empareja las variables segun su posicion en la llamada
-- esto es util a la hora de reemplazar variables cuando tenemos un resultado generico 
pairGenericVars :: [VarT] -> [VarT] -> [(VarT, VarT)]
pairGenericVars [] xss = []
pairGenericVars xs [] = []
pairGenericVars ((Value s):xs) ((Value y):xss) = pairGenericVars xs xss
pairGenericVars ((Value s):xs) ((Generic y):xss) = ((Value s),(Generic y)):pairGenericVars xs xss
pairGenericVars ((Equation s):xs) ((Generic y):xss) = ((Equation s),(Generic y)):pairGenericVars xs xss
pairGenericVars ((List s):xss) ((HeadTail x xs):xsss) = (head s,x):((List (tail s)),xs):pairGenericVars xss xsss
pairGenericVars (x:xs) (y:ys) = pairGenericVars xs ys

-- compareVars: Dada una variable generica y el mapa generado en pairGenericVars, buscamos la variable 
-- y devolvemos el valor a reemplazar
compareVars :: VarT -> [(VarT, VarT)] -> VarT
compareVars var [] = var
compareVars var ((value, generic):xs) = if var == generic then value else compareVars var xs

-- compareEquationVars: Dada una variable de ecuacion y el mapa generado en pairGenericVars, reemplaza
-- las variables dentro de la ecuación por el valor correspondiente
compareEquationVars :: EqToken -> [(VarT, VarT)] -> EqToken
compareEquationVars a [] = a
compareEquationVars (VarNum s) ((Equation e, Generic g):xs) | s == g = case evalNum e of 
                                                                                            Just ev -> Num ev
                                                                                            Nothing -> VarNum s
                                                                              | otherwise = compareEquationVars (VarNum s) xs
compareEquationVars a (x:xs) = compareEquationVars a xs

-- searchForVarsInEq: Dada una ecuacion y el mapa generado en pairGenericVars, reemplaza
-- las variables dentro de la ecuacion recursivamente
searchForVarsInEq :: EqToken -> [(VarT, VarT)] -> EqToken
searchForVarsInEq (VarNum x) map = compareEquationVars (VarNum x) map
searchForVarsInEq (Plus a b) map = Plus (searchForVarsInEq a map) (searchForVarsInEq b map)
searchForVarsInEq (Minus a b) map = Minus (searchForVarsInEq a map) (searchForVarsInEq b map)
searchForVarsInEq (NumTimes a b) map = NumTimes (searchForVarsInEq a map) (searchForVarsInEq b map)
searchForVarsInEq (NumDiv a b) map = NumDiv (searchForVarsInEq a map) (searchForVarsInEq b map)
searchForVarsInEq a map = a

-- replaceGenericVars: Dada una lista de variables y el mapa generado en pairGenericVars, reemplaza 
-- las variables dentro de las ecuaciones y las variables genericas por los valores correspondientes
replaceGenericVars :: [VarT] -> [(VarT, VarT)] -> Maybe [VarT]
replaceGenericVars [] xs = return []
replaceGenericVars ((Generic s):xs) xss = do rest <- replaceGenericVars xs xss
                                             return ((compareVars (Generic s) xss):rest)
replaceGenericVars ((Equation eq):xs) xss = do  rest <- replaceGenericVars xs xss 
                                                let eqT = searchForVarsInEq eq xss
                                                evalEq <- evalNum eqT
                                                return ((Equation (Num evalEq)):rest)
replaceGenericVars (x:xs) xss = do rest <- replaceGenericVars xs xss
                                   return (x:rest)

-- replaceFunctionVars: Dado el mapa generado en pairGenericVars y un expresión, reemplaza
-- las variables dentro de la expresión recursivamente
replaceFunctionVars :: [(VarT, VarT)] -> Exp -> Maybe Exp
replaceFunctionVars vars (Fun name fvars) = do fvars <- (replaceGenericVars fvars vars)
                                               return (Fun name fvars)
replaceFunctionVars vars (Var (Equation s)) = do let eqT = searchForVarsInEq s vars
                                                 evalEq <- evalNum eqT
                                                 return (Var (Equation (Num evalEq)))
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
replaceFunctionVars vars (Times a b) = do fa <- (replaceFunctionVars vars a)
                                          fb <- (replaceFunctionVars vars b)
                                          return (Times fa fb)  
replaceFunctionVars vars (Div a b) = do fa <- (replaceFunctionVars vars a)
                                        fb <- (replaceFunctionVars vars b)
                                        return (Div fa fb)  
replaceFunctionVars vars exp = return exp

-- searchForMatch: Dada una key, un entorno y una flag devuelve la expresión asociada a esa key.
-- Siempre se prioriza obtener un resultado booleano, es decir si tengo una función 
-- que resulta en true o false, se devuelve eso aunque tenga otras definiciones. 
-- Luego se buscan matches perfectos con las variables dadas, si eso no se encuentra,
-- se devuelve un match genérico. 
-- Si se da el caso de un match genérico, se vuelve a hacer una busqueda con el resultado y las 
-- variables reemplazadas, si se da el mismo resultado, quiere decir que va a entrar en un bucle
-- ya que no hay un resultado válido al final de la cadena de ejecución y que
-- la expresión que se consiguió es la única válida, para eso se utiliza
-- la flag depth, si esta en True se hace solo una busqueda de un nivel, si esta en False,
-- se hace este checkeo
searchForMatch:: Key -> [(Key, Exp)] -> Bool -> Maybe Exp
searchForMatch search envlist depth = let (match, generic) = functionMatcher search envlist
                                      in case (searchResult ((map Prelude.fst match) ++ (map Prelude.fst generic))) of
                                              Just x -> Just x
                                              Nothing -> case match of 
                                                          [] -> case generic of 
                                                                  [] -> Nothing
                                                                  xs -> let (exp, vars) = head xs
                                                                            varsPair = pairGenericVars (getVars search) vars
                                                                            searchResultM = replaceFunctionVars varsPair exp
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

-- prepareVarsForGenericSearch: Para el caso de la busqueda genérica, se reemplazan
-- las variables genericas por un token Joker para representar que puede ir cualquier
-- valor en esa posición
prepareVarsForGenericSearch :: [VarT] -> [VarT]
prepareVarsForGenericSearch [] = []
prepareVarsForGenericSearch ((Generic a):xs) = (Joker a):prepareVarsForGenericSearch xs 
prepareVarsForGenericSearch (x:xs) = x:prepareVarsForGenericSearch xs 

-- searchForGenerics: Dada una Key con variables genericas y un entorno se realiza la busqueda generica.
-- Esto devuelve todos los valores que pueden tomar las variables genericas para retornar true
-- No se tienen en cuenta definiciones recursivas
searchForGenerics:: Key -> [(Key, Exp)] -> Bool -> Maybe Exp
searchForGenerics search envlist depth = let  vars = prepareVarsForGenericSearch (getVars search)
                                              name = getName search
                                              (matches, _) = functionMatcher (name, vars) envlist
                                              varArr = map Prelude.snd matches
                                         in case matches of
                                              [] -> Nothing
                                              xs -> Just (ReturnVars (transpose ((getVars search):varArr)))
-- searchType: Revisa si dentro de la lista de variables hay genericas, si ese es el caso
-- devuelvo False para hacer una busqueda generica, si no devuelve True y hace una busqueda normal
searchType :: [VarT] -> Bool
searchType [] = True
searchType ((Generic _):xs) = False
searchType (x:xs) = searchType xs

-- functionSearch: Dada una key y un entorno, devuelve la expresion asociada a esa key
functionSearch :: Key -> [(Key, Exp)] -> Bool -> Maybe Exp
functionSearch search envlist depth = if searchType (getVars search) then
                                         searchForMatch search envlist depth  
                                      else searchForGenerics search envlist depth