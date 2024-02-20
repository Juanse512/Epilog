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
        |   Exp '*' Exp                 {Times $1 $3}
        |   Exp '/' Exp                 {Div $1 $3}
        |   '{' nums '}'                {Var (VarN (Equation $2))}
        
vars :: { [ VarT ] }
vars    :   eqs                        {[$1]}
        |   lst                        {[$1]}
        |   VAR                        {[generateVar $1]}
        |   vars ',' eqs               {$3 : $1}
        |   vars ',' VAR               {(generateVar $3) : $1}
        |   vars ',' VAR               {$1 ++ [(generateVar $3)]}
        |   vars ',' lst               {$3 : $1}

lst :: { VarT }
lst     : '[' varlst ']'               {VarN (List $2)}
        | '[' ']'                      {VarN (List [])}
        | VAR ':' VAR                  {VarN (HeadTail (Generic $1) (Generic $3))}

eqs :: { VarT }
eqs     : '{' nums '}'                   {VarN (Equation $2)}

nums :: { [EqToken] }
nums    : VAR                           {lexNum $1}
        | nums '+' nums                 {$1 ++ [Plus] ++ $3}
        | nums '-' nums                 {$1 ++ [Minus] ++ $3}
        | nums '*' nums                 {$1 ++ [NumTimes] ++ $3}
        | nums '/' nums                 {$1 ++ [NumDiv] ++ $3}

varlst :: { [VarT] }
        : VAR                          {[generateVar $1]}
        | varlst ',' VAR               {$1 ++ [(generateVar $3)]}


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
                                if ((bracketIdx == Nothing) || (bracketIdx >= openParIdx)) && openParIdx /= Nothing && openParIdx < closeParIdx && ((openParIdx < enterIdx && enterIdx /= Nothing) || enterIdx == Nothing) then
                                        (TFun (takeWhile (/= '(') cs)) : lexer ((dropWhile (/= '(')) cs)
                                else
                                        case span (\x -> ((isAlpha x || isDigit x || x == '\'') && x /= '\n')) cs of
                                                ("true", rest) -> TTrue : (lexer rest)
                                                ("false", rest) -> TFalse : (lexer rest)
                                                (cs, rest) -> if bracketIdx /= Nothing && ((bracketIdx <= bracketOpenIdx) || (bracketOpenIdx == Nothing))
                                                              then let var = getEq cs in (TVar var) : lexer (getRest rest)
                                                              else let var = getVar cs in (TVar var) : lexer (getRest rest)
     
                getVar [] = []
                getVar (')':cs) = []
                getVar (',':cs) = []
                getVar ('(':cs) = []
                getVar (':':cs) = []
                getVar (' ':cs) = []
                getVar (c:cs) = c : (getVar cs)
                
                getEq [] = []
                getEq (')':cs) = []
                getEq ('(':cs) = []
                getEq (':':cs) = []
                getEq (c:cs) = c : (getEq cs)

                getRest [] = []
                getRest (')':cs) = ')':cs
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