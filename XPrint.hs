module XPrint where

import XExpression
import XStatement
import Data.List

indent :: Int -> String
indent n = take (2*n) $ repeat ' '

bracket :: String -> String
bracket s = "(" ++ s ++ ")"

braces :: String -> String
braces s = "{" ++ s ++ "}"

-- tabbify :: Int -> (a -> String) -> (a -> String)
-- tabbify i f = \e -> (indent i) ++ (f e)

showExpr :: XExpression -> String
showExpr (XFuncCall f es) = f ++ bracket (show $ map showExpr es)
showExpr (XVarRefIdent v) = v
showExpr (XVarIdx v e)    = v ++ "[" ++ (showExpr e) ++ "]"
showExpr (XVarMember v m) = v ++ "." ++ (showExpr m)
showExpr (XConstInt i)    = show i
showExpr (XConstChar c)   = "'" ++ (show c) ++ "'"
showExpr (XConstEnum e)   = e
showExpr (XString s)      = "\"" ++ s ++ "\""
showExpr (XAdd a b)       = bracket (showExpr a) ++ " + " ++ bracket (showExpr b)
showExpr (XSub a b)       = bracket (showExpr a) ++ " - " ++ bracket (showExpr b)
showExpr (XMul a b)       = bracket (showExpr a) ++ " * " ++ bracket (showExpr b)
showExpr (XDiv a b)       = bracket (showExpr a) ++ " / " ++ bracket (showExpr b)
showExpr (XMod a b)       = bracket (showExpr a) ++ " % " ++ bracket (showExpr b)
showExpr (XNeg e)         = "-" ++ bracket (showExpr e)
showExpr (XPos e)         = "+" ++ bracket (showExpr e)
showExpr (XBitNot e)      = "~" ++ bracket (showExpr e)
showExpr (XLogicNot e)    = "!" ++ bracket (showExpr e)
showExpr (XRef e)         = "&" ++ bracket (showExpr e)
showExpr (XDeref e)       = "*" ++ bracket (showExpr e)
showExpr (XPostInc e)     = bracket (showExpr e) ++ "++"
showExpr (XPostDec e)     = bracket (showExpr e) ++ "--"
showExpr (XPreInc e)      = "++" ++ bracket (showExpr e)
showExpr (XPreDec e)      = "--" ++ bracket (showExpr e)
showExpr (XLShift a b)    = bracket (showExpr a) ++ " << " ++ bracket (showExpr b)
showExpr (XRShift a b)    = bracket (showExpr a) ++ " >> " ++ bracket (showExpr b)
showExpr (XEq a b)        = bracket (showExpr a) ++ " == " ++ bracket (showExpr b)
showExpr (XNeq a b)       = bracket (showExpr a) ++ " != " ++ bracket (showExpr b)
showExpr (XLess a b)      = bracket (showExpr a) ++ " < " ++ bracket (showExpr b)
showExpr (XGreater a b)   = bracket (showExpr a) ++ " > " ++ bracket (showExpr b)
showExpr (XLeq a b)       = bracket (showExpr a) ++ " <= " ++ bracket (showExpr b)
showExpr (XGeq a b)       = bracket (showExpr a) ++ " >= " ++ bracket (showExpr b)
showExpr (XAssign v e)    = showExpr v ++ " = " ++ bracket (showExpr e)
showExpr (XCast t e)      = bracket t ++ bracket (showExpr e)

showStmt :: XStatement -> String
showStmt (XStatement.Nil)             = ""
showStmt (XIfElse e s1 Nothing)       = "if" ++ (bracket $ showExpr e) ++ showStmt s1
showStmt (XIfElse e s1 (Just s2))     = (showStmt (XIfElse e s1 Nothing)) ++ "else" ++ showStmt s2
showStmt (XWhile e s)                 = "while" ++ (bracket $ showExpr e) ++ showStmt s
showStmt (XFor a b c s)               = "for" ++ (bracket $ showStmt a) ++ showStmt b ++ showExpr c ++ showStmt s
showStmt (XPar s)                     = "par" ++ showStmt s
showStmt (XTypeDef a b)               = "typedef " ++ a ++ " " ++ b ++ ";"
showStmt (XReturn e)                  = "return " ++ showExpr e ++ ";"
showStmt (XBreak)                     = "break;"
showStmt (XContinue)                  = "continue;"
showStmt (XStmtSeq [])                = "{}"
showStmt (XStmtSeq ss)                = braces $ foldr1 (++) $ map showStmt ss
showStmt (XExpStmt e)                 = showExpr e ++ ";"
showStmt (XFuncDecl (t, n) params []) = t ++ " " ++ n ++ "(void){}"
showStmt (XFuncDecl (t, n) params ss) = t ++ " " ++ n ++ "(void)" ++ (braces $ foldr1 (++) $ map showStmt ss)
showStmt s = show s ++ " "
