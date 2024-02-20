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
data Name = Generic String
          | Value String
          | Function String
          | ReturnValue Bool
          | Joker String
          | Equation EqToken
          | List [VarT]
          | HeadTail Name Name
    deriving (Show, Eq)

data VarT = VarN Name deriving (Show, Eq)

data Exp = Fun Name [VarT]
          | Assgn Name [VarT] Exp
          | Var VarT
          | Seq Exp Exp
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
           | VarRes Name

-- Entornos
type NameEnv = [(Exp, [Name], Exp)]


data Error = UndefVar | InvalidOp | WrongDef deriving (Eq, Show)


generateVar :: String -> VarT
generateVar ('\'':cs) = VarN (Value (takeWhile (/= '\'') cs))
generateVar cs = case cs of 
                    "true" -> VarN (ReturnValue True)
                    "false" -> VarN (ReturnValue False)
                    _ -> VarN (Generic cs)
-- gato(tom) :- true.

-- padrede('Juan', 'María').

-- hijode(A,B) :- padrede(B,A).
-- hermanode(A,B) :- 
--    padrede(C,A) , 
--    padrede(C,B), 
--    A \== B. 

-- ?- hermanode('Juan', 'Marcela').
-- yes