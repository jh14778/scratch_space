module XCAST where

data XExpression = Nil |
  XFuncCall String [XExpression] |
  XPostInc XExpression |
  XPostDec XExpression |
  XVarRefIdent String |
  XVarIdx String XExpression |
  XVarMember String XExpression |
  XConstInt Integer |
  XConstChar Char |
  XConstEnum String |
  XString String |
  XAdd XExpression XExpression |
  XSub XExpression XExpression
  deriving Show
