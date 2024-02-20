module Common where
-- Comandos interactivos o de archivos
data Stmt i = Def String i | Eval i deriving Show

instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

data EqToken = Plus EqToken EqToken 
             | Minus EqToken EqToken
             | NumTimes EqToken EqToken
             | NumDiv EqToken EqToken
             | VarNum String 
             | Num Int deriving (Show, Eq)

-- Tipos de los nombres
data VarT = Generic String
          | Value String
          | Function String
          | ReturnValue Bool
          | Joker String
          | Equation EqToken
          | List [VarT]
          | HeadTail VarT VarT
    deriving (Show, Eq)

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

data Value = Val Bool
           | VarRes VarT

-- Entornos
type Key = (VarT, [VarT])
type Env = [(Key, Exp)]

data Error = UndefVar | InvalidOp | WrongDef deriving (Eq, Show)


generateVar :: String -> VarT
generateVar ('\'':cs) = Value (takeWhile (/= '\'') cs)
generateVar cs = case cs of 
                    "true" -> ReturnValue True
                    "false" -> ReturnValue False
                    _ -> Generic cs
-- gato(tom) :- true.

-- padrede('Juan', 'María').

-- hijode(A,B) :- padrede(B,A).
-- hermanode(A,B) :- 
--    padrede(C,A) , 
--    padrede(C,B), 
--    A \== B. 

-- ?- hermanode('Juan', 'Marcela').
-- yes