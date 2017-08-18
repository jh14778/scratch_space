module XExpression where

data XExpression = Nil |
  XFuncCall String [XExpression] |
  XVarRefIdent String |
  XVarIdx String XExpression |
  XVarMember String XExpression |
  XConstInt Integer |
  XConstChar Char |
  XConstEnum String |
  XString String |
  XAdd XExpression XExpression |
  XSub XExpression XExpression |
  XMul XExpression XExpression |
  XDiv XExpression XExpression |
  XMod XExpression XExpression |
  XNeg XExpression |
  XPos XExpression |
  XBitNot XExpression |
  XLogicNot XExpression |
  XRef XExpression |
  XDeref XExpression |
  XPostInc XExpression |
  XPostDec XExpression |
  XPreInc XExpression |
  XPreDec XExpression |
  XLShift XExpression XExpression |
  XRShift XExpression XExpression |
  XEq XExpression XExpression |
  XNeq XExpression XExpression |
  XLess XExpression XExpression |
  XGreater XExpression XExpression |
  XLeq XExpression XExpression |
  XGeq XExpression XExpression
  deriving Show
