module XLexer where

import XUtils
import Data.List
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

reserved_names = unique $ ["break","continue","do","par","if","for","return","enum","struct","union","else","interface","typedef"]
reserved_ops   = unique $ ["-","~","!","&","*","++","--","+","*","/","%","--","<<",">>","==","!=","<",">","<=",">=","=",","]

lexer = Token.makeTokenParser $ emptyDef {
    Token.commentStart    = "/*",
    Token.commentEnd      = "*/",
    Token.commentLine     = "//",
    Token.nestedComments  = False,
    Token.reservedNames   = reserved_names,
    Token.reservedOpNames = reserved_ops
  }

identifier = Token.identifier lexer
intLit     = Token.integer lexer
octLit     = Token.octal lexer
hexLit     = Token.hexadecimal lexer
naturalLit = Token.natural lexer
charLit    = Token.charLiteral lexer
stringLit  = Token.stringLiteral lexer
parens     = Token.parens lexer
squares    = Token.squares lexer
commaSep   = Token.commaSep lexer
semiSep    = Token.semiSep lexer
reservedOp = Token.reservedOp lexer
reserved   = Token.reserved lexer
braces     = Token.braces lexer
semi       = Token.semi lexer
