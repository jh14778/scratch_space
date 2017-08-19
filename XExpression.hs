module XExpression where

import XLexer
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

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
  XGeq XExpression XExpression |
  XAssign XExpression XExpression
  deriving Show

type XExpParser = Parsec String () XExpression

expression :: XExpParser
expression = buildExpressionParser table term <?> "expression"

-- TODO: Deal with operator precedence
table = [
    [Prefix (reservedOp "-" >> return XNeg), Prefix (reservedOp "+" >> return XPos)],
    [Prefix (reservedOp "~" >> return XBitNot), Prefix (reservedOp "!" >> return XLogicNot)],
    [Prefix (reservedOp "&" >> return XRef), Prefix (reservedOp "*" >> return XDeref)],
    [Prefix (reservedOp "++" >> return XPreInc), Prefix (reservedOp "--" >> return XPreDec)],
    [Infix (reservedOp "+" >> return XAdd) AssocLeft, Infix (reservedOp "-" >> return XSub) AssocLeft],
    [Infix (reservedOp "*" >> return XMul) AssocLeft, Infix (reservedOp "/" >> return XDiv) AssocLeft, Infix (reservedOp "%" >> return XMod) AssocLeft],
    [Postfix (reservedOp "++" >> return XPostInc), Postfix (reservedOp "--" >> return XPostDec)],
    [Infix (reservedOp "<<" >> return XLShift) AssocLeft, Infix (reservedOp ">>" >> return XRShift) AssocLeft],
    [Infix (reservedOp "==" >> return XEq) AssocLeft, Infix (reservedOp "!=" >> return XNeq) AssocLeft],
    [Infix (reservedOp "<" >> return XLess) AssocLeft, Infix (reservedOp ">" >> return XGreater) AssocLeft],
    [Infix (reservedOp "<=" >> return XLeq) AssocLeft, Infix (reservedOp ">=" >> return XGeq) AssocLeft],
    [Infix (reservedOp "=" >> return XAssign) AssocRight]
  ]

term :: XExpParser
term = parens expression <|> try func_call <|>
  var_ref <|> constant <|> xstring

xstring :: XExpParser
xstring = XString <$> stringLit

constant :: XExpParser
constant = XConstChar <$> charLit <|> XConstInt <$> (intLit <|> octLit <|> hexLit)

func_call :: XExpParser
func_call = XFuncCall <$> identifier <*> (parens . commaSep $ expression)

var_ref :: XExpParser
var_ref = identifier >>= \ident -> choice [
    squares expression >>= return.(XVarIdx ident),
    char '.' >> var_ref >>= return.(XVarMember ident),
    return $ XVarRefIdent ident
  ]
