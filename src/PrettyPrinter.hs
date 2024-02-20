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
-- pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
-- pp _  _  (Free  (Global s)) = text s
-- pp _  _  Unit               = text "unit"
-- pp ii vs (i :@: c         ) = sep
--   [ parensIf (isLam i || isLet i) (pp ii vs i)
--   , nest 1 (parensIf (isCompound c) (pp ii vs c))
--   ]
-- pp ii vs (Lam t c) =
--   text "\\"
--     <> text (vs !! ii)
--     <> text ":"
--     <> printType t
--     <> text ". "
--     <> pp (ii + 1) vs c
-- pp ii vs (Let t u) =
--   text "let"
--     <+> text (vs !! ii)
--     <+> text "="
--     <+> pp ii vs t
--     <+> text "in"
--     <+> pp (ii + 1) vs u
-- pp ii vs (Pair t u) =
--   text "("
--     <> pp ii vs t
--     <> text ", "
--     <> pp ii vs u
--     <> text ")"
-- pp ii vs (Fst t)  = text "fst" <+> parensIf (isCompound t) (pp ii vs t)
-- pp ii vs (Snd t)  = text "snd" <+> parensIf (isCompound t) (pp ii vs t)
-- pp ii vs Zero     = text "0"
-- pp ii vs (Suc t)  = case toInt t of
--   Just n  -> text $ show (n + 1)
--   Nothing -> text "suc" <+> parensIf (isCompound t) (pp ii vs t)
-- pp ii vs (Rec t1 t2 t3) =
--   text "rec"
--     <+> parensIf (isCompound t1) (pp ii vs t1)
--     <+> parensIf (isCompound t1) (pp ii vs t2)
--     <+> parensIf (isCompound t1) (pp ii vs t3)

renderExp :: Exp -> String
renderExp = render . pp