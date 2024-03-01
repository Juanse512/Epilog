{
module Parse where
import Common
import Data.Maybe
import Data.Char
import Data.List
import Equations
}     
%monad { P } { thenP } { returnP }
%name parse
%error { parseError }

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
    '*'     {TTimes}
    '/'     {TDiv}
    ':'     {TDDot}
    '['     {TLOpen}
    ']'     {TLClose}
    VAR     {TVar $$}
    FUN     {TFun $$}
-- Asignar ordenes
%nonassoc '.' 
%right ':='
%left '+' '-'
%left '*' '/'
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
        |   Exp '==' Exp                {Eq $1 $3}
        |   Exp '!=' Exp                {NEq $1 $3}
        |   Exp '&' Exp                 {And $1 $3}
        |   Exp '|' Exp                 {Or $1 $3}
        |   Exp '+' Exp                 {Add $1 $3}
        |   Exp '-' Exp                 {Sub $1 $3}
        |   Exp '*' Exp                 {Times $1 $3}
        |   Exp '/' Exp                 {Div $1 $3}
        |   '{' nums '}'                {Var (Equation $2)}
        |   VAR                         {Var (generateVar $1)}
        
vars :: { [ VarT ] }
vars    :   eqs                        {[$1]}
        |   lst                        {[$1]}
        |   VAR                        {[generateVar $1]}
        |   eqs ',' vars               {$1 : $3}
        |   VAR ',' vars               {(generateVar $1) : $3 }
        |   lst ',' vars               {$1 : $3}

lst :: { VarT }
lst     : '[' vars ']'               {List $2}
        | '[' ']'                      {List []}
        | VAR ':' VAR                  {HeadTail (Generic $1) (Generic $3)}

eqs :: { VarT }
eqs     : '{' nums '}'                   {Equation $2}

nums :: { EqToken }
nums    : VAR                           {lexNum $1}
        | nums '+' nums                 {Plus $1 $3}
        | nums '-' nums                 {Minus $1 $3}
        | nums '*' nums                 {NumTimes $1 $3}
        | nums '/' nums                 {NumDiv $1 $3}

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
parseError tokens = failP "Error de Parseo"

data Token = TVar String
           | TFun String
           | TOpen
           | TClose
           | TComa
           | TAssgn 
           | TEq
           | TNeq
           | TAnd
           | TOr
           | TTrue
           | TFalse
           | TDot
           | TEOF
           | TCBrace
           | TOBrace
           | TAdd
           | TMinus
           | TDDot
           | TLOpen
           | TLClose
           | TTimes
           | TDiv
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('\n':s) = lexer s
lexer ('.':cs) = TDot : (lexer cs)
lexer ('+':cs) = TAdd : (lexer cs)
lexer ('-':cs) = TMinus : (lexer cs)
lexer ('/':cs) = TDiv : (lexer cs)
lexer ('*':cs) = TTimes : (lexer cs)
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
lexer (':':cs) = TDDot : (lexer cs)
lexer ('[':cs) = TLOpen : (lexer cs)
lexer (']':cs) = TLClose : (lexer cs)
lexer (c:cs) | isSpace c = lexer cs
             | otherwise = lexVar (c:cs)
        where   lexVar :: String -> [Token]
                lexVar cs = let openParIdx = (elemIndex '(' cs)
                                closeParIdx = (elemIndex ')' cs)
                                enterIdx = (elemIndex '\n' cs)
                                bracketIdx = (elemIndex '}' cs)
                                bracketOpenIdx = (elemIndex '{' cs)
                            in
                                -- Si tengo paréntesis, tengo una función
                                if ((bracketIdx == Nothing) || (bracketIdx >= openParIdx)) && openParIdx /= Nothing && openParIdx < closeParIdx && ((openParIdx < enterIdx && enterIdx /= Nothing) || enterIdx == Nothing) then
                                        (TFun (takeWhile (/= '(') cs)) : lexer ((dropWhile (/= '(')) cs)
                                else
                                        -- Obtengo todo el string valido
                                        case span (\x -> ((isAlpha x || isDigit x || x == '\'') && x /= '\n')) cs of
                                                ("true", rest) -> TTrue : (lexer rest)
                                                ("false", rest) -> TFalse : (lexer rest)
                                                        -- Si tengo {} estoy dentro de una ecuacion, si no tengo una variable
                                                (cs, rest) -> if bracketIdx /= Nothing && ((bracketIdx <= bracketOpenIdx) || (bracketOpenIdx == Nothing))
                                                              then let var = getEq cs in (TVar var) : lexer (getRest rest)
                                                              else let var = getVar cs in (TVar var) : lexer (getRest rest)
                -- Obtiene el nombre de una variable
                getVar [] = []
                getVar (')':cs) = []
                getVar (',':cs) = []
                getVar ('(':cs) = []
                getVar (':':cs) = []
                getVar (' ':cs) = []
                getVar (c:cs) = c : (getVar cs)
                
                -- Obtiene una ecuación
                getEq [] = []
                getEq (')':cs) = []
                getEq ('(':cs) = []
                getEq (':':cs) = []
                getEq (c:cs) = c : (getEq cs)
                
                -- Descarta el nombre de una variable y obtiene el resto del string
                getRest [] = []
                getRest (')':cs) = ')':cs
                getRest ('.':cs) = '.':cs
                getRest (',':cs) = ',':cs
                getRest ('(':cs) = '(':cs
                getRest (' ':cs) = ' ':cs
                getRest ('}':cs) = '}':cs
                getRest (']':cs) = ']':cs
                getRest (':':cs) = ':':cs
                getRest ('+':cs) = '+':cs
                getRest ('-':cs) = '-':cs
                getRest ('*':cs) = '*':cs
                getRest ('/':cs) = '/':cs
                getRest (c:cs) = (getRest cs)
}