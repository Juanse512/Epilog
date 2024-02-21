module Common where
-- Comandos interactivos o de archivos
data Stmt i = Def String i | Eval i deriving Show

instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

-- Tipos de tokens en ecuaciones aritméticas
data EqToken = Plus EqToken EqToken 
             | Minus EqToken EqToken
             | NumTimes EqToken EqToken
             | NumDiv EqToken EqToken
             | VarNum String 
             | Num Int deriving (Show, Eq)

-- Tipos de variables
data VarT = Generic String
          | Value String
          | Function String
          | ReturnValue Bool
          | Joker String
          | Equation EqToken
          | List [VarT]
          | HeadTail VarT VarT
    deriving (Show, Eq)
-- Tipos de expresiones
data Exp = Fun VarT [VarT]
          | Assgn VarT [VarT] Exp
          | Var VarT
          | Eq Exp Exp
          | NEq Exp Exp
          | And Exp Exp
          | Or Exp Exp
          | Add Exp Exp
          | Sub Exp Exp
          | Times Exp Exp
          | Div Exp Exp
          | ReturnVars [[VarT]]
          | RTrue
          | RFalse
          | Skip
    deriving (Show, Eq)


-- Entornos
type Key = (VarT, [VarT])
type Env = [(Key, Exp)]

data Error = UndefVar | InvalidOp deriving (Eq, Show)

-- generateVar: Dado un string devuelve el tipo de variable correspondiente a este.
generateVar :: String -> VarT
generateVar ('\'':cs) = Value (takeWhile (/= '\'') cs)
generateVar cs = case cs of 
                    "true" -> ReturnValue True
                    "false" -> ReturnValue False
                    _ -> Generic cs
