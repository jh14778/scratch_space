module Main where

import XExpression
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

reserved_names = []

lexer = Token.makeTokenParser $ emptyDef {
    Token.commentStart   = "/*",
    Token.commentEnd     = "*/",
    Token.commentLine    = "//",
    Token.nestedComments = False,
    Token.reservedNames  = reserved_names
  }

identifier = Token.identifier lexer
intLit     = Token.integer lexer
octLit     = Token.octal lexer
hexLit     = Token.hexadecimal lexer
charLit    = Token.charLiteral lexer
stringLit  = Token.stringLiteral lexer
parens     = Token.parens lexer
squares    = Token.squares lexer
commaSep   = Token.commaSep lexer
reservedOp = Token.reservedOp lexer

expression :: Parsec String u XExpression
expression = buildExpressionParser table term <?> "expression"

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
    [Infix (reservedOp "<=" >> return XLeq) AssocLeft, Infix (reservedOp ">=" >> return XGeq) AssocLeft]
  ]

term :: Parsec String u XExpression
term = parens expression <|> try func_call <|>
  var_ref <|> constant <|> xstring

xstring :: Parsec String u XExpression
xstring = stringLit >>= return.XString

constant :: Parsec String u XExpression
constant = (XConstChar <$> charLit) <|>
  (XConstInt <$> (intLit <|> octLit <|> hexLit))

func_call :: Parsec String u XExpression
func_call = XFuncCall <$> identifier <*> (parens.commaSep) expression

var_ref :: Parsec String u XExpression
var_ref = identifier >>= \ident -> choice [
    squares expression >>= return.(XVarIdx ident),
    char '.' >> var_ref >>= return.(XVarMember ident),
    return $ XVarRefIdent ident
  ]

main :: IO ()
main = parseTest (expression) "foo(bar)+4+2"
