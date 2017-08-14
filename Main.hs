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
parens     = Token.parens lexer
squares    = Token.squares lexer
commaSep   = Token.commaSep lexer

constant :: Parsec String u XConstant
constant = (charLit >>= return.XConstChar) <|>
  (identifier >>= return.XConstEnum) <|>
  ((intLit <|> octLit <|> hexLit) >>= return.XConstInt)

expression :: Parsec String u XExpression
expression = return Nil

func_call :: Parsec String u XExpression
func_call = do
  ident <- identifier
  params <- parens $ commaSep expression
  return $ XFuncCall ident params

post_exp :: Parsec String u XPostExp
post_exp = var_ref >>= \var -> choice [
    string "++" >> return (XPostInc var),
    string "--" >> return (XPostDec var)
  ]

var_ref :: Parsec String u XVarRef
var_ref = identifier >>= \ident -> choice [
    squares expression >>= return.(XVarIdx ident),
    char '.' >> var_ref >>= return.(XVarMember ident),
    return $ XVarRefIdent ident
  ]

main :: IO ()
main = parseTest func_call "foo()"
