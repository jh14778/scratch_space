module XCAST where

data XConstant = XConstInt Integer | XConstChar Char | XConstEnum String deriving Show
data XVarRef = XVarRefIdent String | XVarIdx String XExpression | XVarMember String XVarRef deriving Show
data XPostExp = XPostInc XVarRef | XPostDec XVarRef deriving Show
data XExpression = Nil | XFuncCall String [XExpression] deriving Show
