module XStatement where

import XExpression
import XLexer
import XUtils
import Text.Parsec
import Text.Parsec.Language

data XStatement =
  Nil |
  XStmtSeq [XStatement] |
  XIfElse XExpression XStatement (Maybe XStatement) |
  XWhile XExpression XStatement |
  XDoWhile XStatement XExpression |
  XFor XStatement XExpression XStatement XStatement |
  XPar [XStatement] |
  XExpStmt XExpression |
  XEnumDecl (Maybe String) [String] |
  XStructDecl (Maybe String) [(String,String)] |
  XUnionDecl (Maybe String) [(String,String)] |
  XFuncDecl (String,String) [(String,Maybe String)] [XStatement]
  deriving Show

type XStmtParser = Parsec String () XStatement

statement :: XStmtParser
statement = return XStatement.Nil

whileLoop :: XStmtParser
whileLoop = reserved "while" >> XWhile <$> (parens expression) <*> (braces statement)

ifElseStmt :: XStmtParser
ifElseStmt = reserved "if" >> XIfElse <$> (parens expression) <*> (braces statement) <*> (maybeParse (reserved "else" >> braces statement))

forLoop :: XStmtParser
forLoop = reserved "for" >> parens (XFor <$> statement <*> expression <*> statement) <*> (braces statement)

doWhile :: XStmtParser
doWhile = reserved "do" >> XDoWhile <$> (braces statement) <*> (reserved "while" >> parens expression)

par :: XStmtParser
par = reserved "par" >> XPar <$> (braces $ semiSep statement)

expStmt :: XStmtParser
expStmt = XExpStmt <$> expression

enumDecl :: XStmtParser
enumDecl = reserved "enum" >> XEnumDecl <$> (maybeParse identifier) <*> (braces $ commaSep identifier)

structDecl :: XStmtParser
structDecl = reserved "struct" >> XStructDecl <$> (maybeParse identifier) <*> (braces $ semiSep varDecl)

unionDecl :: XStmtParser
unionDecl = reserved "union" >> XUnionDecl <$> (maybeParse identifier) <*> (braces $ semiSep varDecl)

funcDecl :: XStmtParser
funcDecl = XFuncDecl <$> varDecl <*> (parens $ commaSep varDecl') <*> (braces $ semiSep statement)

varDecl :: Parsec String () (String, String)
varDecl = tie <$> identifier <*> identifier

varDecl' :: Parsec String () (String, Maybe String)
varDecl' = tie <$> identifier <*> (maybeParse identifier)
