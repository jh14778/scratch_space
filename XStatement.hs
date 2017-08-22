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
  XFor XStatement XStatement XExpression XStatement |
  XPar XStatement |
  XExpStmt XExpression |
  XEnumDecl (Maybe Identifier) [Identifier] |
  XStructDecl (Maybe Identifier) [(Identifier,Identifier)] |
  XUnionDecl (Maybe Identifier) [(Identifier,Identifier)] |
  XInterDecl (Maybe Identifier) [XStatement] |
  XFuncDecl (Identifier,Identifier) [(Identifier,Maybe Identifier)] [XStatement] |
  XVarDecl Identifier [XExpression] |
  XTypeDef Identifier Identifier |
  XReturn XExpression |
  XBreak |
  XContinue
  deriving Show

type XStmtParser = Parsec String () XStatement

externStmt :: XStmtParser
externStmt = preprocessor <|> enumDecl <|> structDecl <|> unionDecl <|> interDecl <|> typedef <|> try varDeclStmt <|> funcDecl

statement :: XStmtParser
statement = ret <|> typedef <|> whileLoop <|> ifElseStmt <|> forLoop <|> doWhile <|> par <|> (reservedOp ";" >> return XStatement.Nil) <|> try varDeclStmt <|> seqStmt <|> expStmt

seqStmt :: XStmtParser
seqStmt = XStmtSeq <$> (braces $ many statement)

whileLoop :: XStmtParser
whileLoop = reserved "while" >> XWhile <$> (parens expression) <*> statement

ifElseStmt :: XStmtParser
ifElseStmt = reserved "if" >> XIfElse <$> (parens expression) <*> statement <*> (maybeParse (reserved "else" >> statement))

forLoop :: XStmtParser
forLoop = reserved "for" >> parens (XFor <$> statement <*> expStmt <*> expression) <*> statement

doWhile :: XStmtParser
doWhile = reserved "do" >> XDoWhile <$> statement <*> (reserved "while" >> parens expression)

par :: XStmtParser
par = reserved "par" >> XPar <$> statement

ret :: XStmtParser
ret = reserved "return" >> XReturn <$> expression <* reservedOp ";"

xbreak :: XStmtParser
xbreak = reserved "break" >> return XBreak <* reservedOp ";"

xcontinue :: XStmtParser
xcontinue = reserved "continue" >> return XContinue <* reservedOp ";"

expStmt :: XStmtParser
expStmt = XExpStmt <$> expression <* reservedOp ";"

enumDecl :: XStmtParser
enumDecl = reserved "enum" >> XEnumDecl <$> (maybeParse identifier) <*> (braces $ commaSep identifier)

structDecl :: XStmtParser
structDecl = reserved "struct" >> XStructDecl <$> (maybeParse identifier) <*> (braces $ semiSep varDecl)

unionDecl :: XStmtParser
unionDecl = reserved "union" >> XUnionDecl <$> (maybeParse identifier) <*> (braces $ semiSep varDecl)

interDecl :: XStmtParser
interDecl = reserved "interface" >> XInterDecl <$> (maybeParse identifier) <*> (braces $ many funcDecl)

funcDecl :: XStmtParser
funcDecl = XFuncDecl <$> varDecl <*> (parens $ commaSep varDecl') <*> ((braces $ many statement) <|> (reservedOp ";" >> return []))

typedef :: XStmtParser
typedef = reserved "typedef" >> XTypeDef <$> identifier <*> identifier <* reservedOp ";"

varDeclStmt :: XStmtParser
varDeclStmt = XVarDecl <$> identifier <*> (commaSep expression) <* reservedOp ";"

preprocessor :: XStmtParser
preprocessor = reservedOp "#" >> (manyTill anyChar newline) >> return XStatement.Nil

varDecl :: Parsec String () (String, String)
varDecl = tie <$> identifier <*> identifier

varDecl' :: Parsec String () (String, Maybe String)
varDecl' = tie <$> identifier <*> (maybeParse identifier)
