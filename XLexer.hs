module XLexer where

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
semiSep    = Token.semiSep lexer
reservedOp = Token.reservedOp lexer
reserved   = Token.reserved lexer
braces     = Token.braces lexer
