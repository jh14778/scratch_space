module Main where

import XCAST
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
expression = try func_call <|> var_ref <|> constant <|> xstring

arith_expr :: Parsec String u XExpression
arith_expr = do
  e1 <- expression
  add_expr e1 <|> sub_expr e1

add_expr :: XExpression -> Parsec String u XExpression
add_expr e1 = reservedOp "+" >> expression >>= return.(XAdd e1)

sub_expr :: XExpression -> Parsec String u XExpression
sub_expr e1 = reservedOp "-" >> expression >>= return.(XSub e1)

xstring :: Parsec String u XExpression
xstring = stringLit >>= return.XString

constant :: Parsec String u XExpression
constant = (charLit >>= return.XConstChar) <|>
  ((intLit <|> octLit <|> hexLit) >>= return.XConstInt)

func_call :: Parsec String u XExpression
func_call = do
  ident <- identifier
  params <- parens $ commaSep expression
  return $ XFuncCall ident params

post_exp :: Parsec String u XExpression
post_exp = var_ref >>= \var -> choice [
    string "++" >> return (XPostInc var),
    string "--" >> return (XPostDec var)
  ]

var_ref :: Parsec String u XExpression
var_ref = identifier >>= \ident -> choice [
    squares expression >>= return.(XVarIdx ident),
    char '.' >> var_ref >>= return.(XVarMember ident),
    return $ XVarRefIdent ident
  ]

main :: IO ()
main = parseTest (expression) "foo(bar)+4+2"
