module Parser
  ( parseString
  ) where

import           Control.Monad.Identity
import           Prelude                hiding (const, lex)
import           Syntax
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language   (javaStyle)
import qualified Text.Parsec.Token      as T

-- Lexer based on:
-- https://github.com/brownplt/webbits/blob/master/src/BrownPLT/JavaScript/Lexer.hs
lex =
  T.makeTokenParser $
  javaStyle
    { T.reservedNames =
        [ "function"
        , "if"
        , "else"
        , "while"
        , "break"
        , "return"
        , "true"
        , "false"
        , "let"
        , "listen"
        , "get"
        , "post"
        ]
    , T.reservedOpNames =
        ["+", "-", "*", "(", ")", "{", "}", ";", ":", ">", "=="]
    }

identifier = T.identifier lex

reserved = T.reserved lex

operator = T.operator lex

reservedOp = T.reservedOp lex

charLiteral = T.charLiteral lex

stringLiteral = T.stringLiteral lex

natural = T.natural lex

integer = T.integer lex

float = T.float lex

naturalOrFloat = T.naturalOrFloat lex

decimal = T.decimal lex

hexadecimal = T.hexadecimal lex

octal = T.octal lex

symbol = T.symbol lex

whiteSpace = T.whiteSpace lex

parens = T.parens lex

braces = T.braces lex

squares = T.squares lex

semi = T.semi lex

comma = T.comma lex

colon = T.colon lex

dot = T.dot lex

brackets = T.brackets lex

lexeme = T.lexeme lex

commaSep = T.commaSep lex

semiSep = T.semiSep lex

-- Parser based on:
--   https://github.com/brownplt/webbits/blob/master/src/BrownPLT/JavaScript/Parser.hs
type Parser a = ParsecT String () Identity a

const :: Parser Const
const =
  fmap (CInt . fromIntegral) integer <|>
  (reserved "true" >> return (CBool True)) <|>
  (reserved "false" >> return (CBool False))

term :: Parser Expr
term =
  parens expr <|> (fmap EConst const) <|>
  (fmap EId identifier) <?> "simple expression"

table =
  [ [binary "+" (EOp2 Add) AssocLeft, binary "-" (EOp2 Sub) AssocLeft]
  , [binary ">" (EOp2 OGT) AssocLeft, binary "==" (EOp2 Eq) AssocLeft]
  ]

binary name fun assoc = Infix (reservedOp name >> return fun) assoc

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

sLet :: Parser Stmt
sLet = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e <- expr
  semi
  return (SLet x (BExpr e))

sLabel :: Id -> Parser Stmt
sLabel x = do
  reservedOp ":"
  s <- stmt
  return (SLabel x s)

sSet :: Id -> Parser Stmt
sSet x = do
  reservedOp "="
  e <- expr
  semi
  return (SSet (LVId x) (BExpr e))

sSetOrLabel :: Parser Stmt
sSetOrLabel = do
  x <- identifier
  sLabel x <|> sSet x

sFunction :: Id -> Parser Stmt
sFunction f = do
  reserved "function"
  args <- parens (commaSep identifier)
  body <- blk
  return (SLet f (BFunc args body))

-- NOTE(emily): I'm sure there is some way to combine these in a better way.
sListen :: Id -> Parser Stmt
sListen x = do
  reserved "listen"
  args <- parens (commaSep expr)
  return (SLet x (BEvent EvListen args))

sGet :: Id -> Parser Stmt
sGet x = do
  reserved "get"
  args <- parens (commaSep expr)
  return (SLet x (BEvent EvGet args))

sPost :: Id -> Parser Stmt
sPost x = do
  reserved "post"
  args <- parens (commaSep expr)
  return (SLet x (BEvent EvPost args))

idFollowedByLParen :: Parser Id
idFollowedByLParen =
  try $ do
    x <- identifier
    lookAhead (reservedOp "(")
    return x

sApp :: Id -> Parser Stmt
sApp r = do
  f <- idFollowedByLParen
  args <- parens (commaSep expr)
  return (SLet r (BApp f args))

sExpr :: Id -> Parser Stmt
sExpr x = do
  e <- expr
  return (SLet x (BExpr e))

sBinding :: Parser Stmt
sBinding = do
  reserved "let"
  x <- identifier
  reservedOp "="
  stmt <- sFunction x <|> sListen x <|> sGet x <|> sPost x <|> sApp x <|> sExpr x
  semi
  return stmt

sIf :: Parser Stmt
sIf = do
  reserved "if"
  e <- parens expr
  s1 <- stmt
  reserved "else"
  s2 <- stmt
  return (SIf e s1 s2)

sWhile :: Parser Stmt
sWhile = do
  reserved "while"
  e <- parens expr
  s <- stmt
  return (SWhile e s)

sBreak :: Parser Stmt
sBreak = do
  reserved "break"
  l <- identifier
  semi
  return (SBreak l)

sReturn :: Parser Stmt
sReturn = do
  reserved "return"
  e <- expr
  semi
  return (SReturn e)

sSeq :: Parser Stmt
sSeq = do
  ss <- braces (many stmt)
  return (SSeq ss)

blk :: Parser Block
blk = do
  ss <- braces (many stmt)
  return (ss)

stmt :: Parser Stmt
stmt =
  sIf <|> sWhile <|> sBreak <|> sReturn <|> sSeq <|> sSetOrLabel <|> sBinding

program :: Parser Stmt
program = whiteSpace >> fmap SSeq (many stmt)

parseString :: String -> Stmt
parseString s =
  case runParser program () "" s of
    Left err      -> error (show err)
    Right program -> program
