{
module Parse where
import Common
import Data.Maybe
import Data.Char
import Data.List
import Helpers
}     
%monad { P } { thenP } { returnP }
%name parse
%error { parseError }
-- %name Exp

%tokentype { Token }


%token
    '('     {TOpen}
    ')'     {TClose}
    ','     {TComa}
    ':='    {TAssgn}
    '!='    {TNeq}
    '=='    {TEq}
    '&'     {TAnd}
    '|'     {TOr}
    'true'  {TTrue}
    'false' {TFalse}
    '.'     {TDot}
    '{'     {TOBrace}
    '}'     {TCBrace}
    '+'     {TAdd}
    '-'     {TMinus}
    VAR     {TVar $$}
    FUN     {TFun $$}
-- Asignar ordenes
%nonassoc '.' 
%right ':='
%%

Exps  :: {[Exp]}
Exps  : Exxp                                      {[$1]}
      | Exxp Exps                                 { $1 : $2 }

Exxp  :: {Exp}
Exxp  : FUN '(' vars ')' ':=' Exp '.'             {Assgn (Function $1) $3 $6}
      | FUN '(' vars ')' ':=' eqs '.'             {Assgn (Function $1) $3 (Var $6)}
      | Exp '.'                                   {$1}
       

Exp     :: {Exp}
        :   'true'                      {RTrue}
        |   'false'                     {RFalse}
        |   FUN '(' vars ')'            {Fun (Function $1) $3}
        |   Exp ',' Exp                 {Seq $1 $3}
        |   Exp '==' Exp                {Eq $1 $3}
        |   Exp '!=' Exp                {NEq $1 $3}
        |   Exp '&' Exp                 {And $1 $3}
        |   Exp '|' Exp                 {Or $1 $3}
        |   Exp '+' Exp                 {Add $1 $3}
        |   Exp '-' Exp                 {Sub $1 $3}
        
vars :: { [ VarT ] }
vars    :   eqs                        {[$1]}
        |   VAR                        {[generateVar $1]}
        |   vars ',' eqs               {$3 : $1}
        |   vars ',' VAR               {(generateVar $3) : $1}

eqs :: { VarT }
eqs     : '{' VAR '}'                   {VarN (Equation (lexNum $2))}



-- AssgnDef :: {Exp}
--          : FUN '(' vars ')' ':=' Exp    {Assgn (Function $1) $3 $6}

-- Defs    : Defexp Defs                  { $1 : $2 }
--         |     

{
data P a = Ok a | Failed String
                    deriving Show

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = case m of
                Ok a     -> k a
                Failed e -> Failed e

returnP :: a -> P a
returnP a = Ok a

failP :: String -> P a
failP err = Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = case m of
                        Ok a     -> Ok a
                        Failed e -> k e

parseError :: [Token] -> P a
parseError tokens = failP "Parse error"

data Token = TVar String
           | TFun String
           | TOpen--
           | TClose--
           | TComa--
           | TAssgn --
           | TEq--
           | TNeq--
           | TAnd--
           | TOr--
           | TTrue
           | TFalse
           | TDot
           | TEOF
           | TCBrace
           | TOBrace
           | TAdd
           | TMinus
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('\n':s) = lexer s
lexer ('.':cs) = TDot : (lexer cs)
lexer ('+':cs) = TAdd : (lexer cs)
lexer ('-':cs) = TMinus : (lexer cs)
lexer (',':cs) = TComa : (lexer cs)
lexer ('(':cs) = TOpen : (lexer cs)
lexer (')':cs) = TClose : (lexer cs)
lexer ('{':cs) = TOBrace : (lexer cs)
lexer ('}':cs) = TCBrace : (lexer cs)
lexer (':':('=':cs)) = TAssgn : (lexer cs)
lexer ('=':('=':cs)) = TEq : (lexer cs)
lexer ('!':('=':cs)) = TNeq : (lexer cs)
lexer ('&':cs) = TAnd : (lexer cs)
lexer ('|':cs) = TOr : (lexer cs)
lexer (c:cs) | isSpace c = lexer cs
             | otherwise = lexVar (c:cs)
        where   lexVar :: String -> [Token]
                lexVar cs = let openParIdx = (elemIndex '(' cs)
                                closeParIdx = (elemIndex ')' cs)
                                enterIdx = (elemIndex '\n' cs)
                            in
                                if openParIdx /= Nothing && openParIdx < closeParIdx && ((openParIdx < enterIdx && enterIdx /= Nothing) || enterIdx == Nothing) then
                                        (TFun (takeWhile (/= '(') cs)) : lexer ((dropWhile (/= '(')) cs)
                                else
                                        case span (\x -> ((isAlpha x || isDigit x || x == '\'' || isMath x) && x /= '\n')) cs of
                                                ("true", rest) -> TTrue : (lexer rest)
                                                ("false", rest) -> TFalse : (lexer rest)
                                                (cs, rest) -> let var = getVar cs in (TVar var) : lexer (getRest rest)
                getVar [] = []
                getVar (')':cs) = []
                getVar (',':cs) = []
                getVar ('(':cs) = []
                getVar (' ':cs) = []
                getVar (c:cs) = c : (getVar cs)
                
                getRest [] = []
                getRest (')':cs) = ')':cs
                getRest (',':cs) = ',':cs
                getRest ('(':cs) = '(':cs
                getRest (' ':cs) = ' ':cs
                getRest ('}':cs) = '}':cs
                getRest (c:cs) = (getRest cs)
                isMath '+' = True
                isMath '-' = True
                isMath _ = False
}