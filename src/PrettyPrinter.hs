module PrettyPrinter where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
-- vars :: [String]
-- vars =
--   [ c : n
--   | n <- "" : map show [(1 :: Integer) ..]
--   , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
--   ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

printVars:: [VarT] -> String
printVars [] = []
printVars [Generic n] = n
printVars [Value n] = "'" ++ n ++ "'"
printVars (var:cs) = case var of 
                          (Generic n) -> n ++ "," ++ (printVars cs)
                          (Value n) -> "'" ++ n ++ "'" ++ "," ++ (printVars cs)

-- printVars (Var (Generic n)):cs = n ++ "," ++ (printVars cs)
-- printVars (Var (Value n)):cs = "'" ++ n ++ "'" ++ "," ++ (printVars cs)
ppEq :: EqToken -> Doc
ppEq (Num i) = text $ show i
ppEq (VarNum i) = text i
ppEq (Plus a b) = ppEq a <+> text "+" <+> ppEq b
ppEq (Minus a b) = ppEq a <+> text "-" <+> ppEq b
ppEq (NumTimes a b) = ppEq a <+> text "*" <+> ppEq b
ppEq (NumDiv a b) = ppEq a <+> text "/" <+> ppEq b

pp :: Exp -> Doc
pp (Fun (Function name) var) = text (name ++ "(" ++ (printVars var) ++ ")")
pp (Assgn (Function name) var exp) = text (name ++ "(" ++ printVars var ++ ")" ++ ":=" ) <+> pp exp
pp (Eq exp exp2) =  pp exp <+> text "==" <+> pp exp2
pp (NEq exp exp2) =  pp exp <+> text "!=" <+> pp exp2
pp (Or exp exp2) =  pp exp <+> text "|" <+> pp exp2
pp (And exp exp2) =  pp exp <+> text "&" <+> pp exp2
pp Skip = text "."
pp (RTrue) = text "true"
pp (RFalse) = text "false"
pp (Var (Equation s)) = ppEq s

renderExp :: Exp -> String
renderExp = render . pp
