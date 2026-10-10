module Parser where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (void)
import Control.Monad.Combinators.Expr

-- Tipo base do Parser
type Parser = Parsec Void String

--------------------------------------------------------------------------------
-- 1. CONSUMIDOR DE ESPAÇOS E LEXEMAS
--------------------------------------------------------------------------------

-- Ignora espaços, tabulações, quebras de linha e comentários
-- cada linha deve ser um statement então não podemos pular linhas
sc :: Parser ()
sc = L.space (void $ oneOf " \t") (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

-- Garante que após ler um token, os espaços à frente sejam consumidos
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Lê uma string exata ignorando espaços ao redor
symbol :: String -> Parser String
symbol = L.symbol sc

--------------------------------------------------------------------------------
-- 2. TIPOS BÁSICOS E LITERAIS
--------------------------------------------------------------------------------

pInteger :: Parser Expr
pInteger = lexeme $ do
    n <- L.signed sc L.decimal
    notFollowedBy (alphaNumChar <|> char '_')
    return (LitInt n)

pFloat :: Parser Expr
pFloat = lexeme $ do
    n <- L.signed sc L.float
    notFollowedBy (alphaNumChar <|> char '_')
    return (LitFloat n)

pBoolean :: Parser Expr
pBoolean = lexeme $ do
    value <- (True <$ string "true") <|> (False <$ string "false")
    notFollowedBy (alphaNumChar <|> char '_')
    return (LitBool value)

pString :: Parser Expr
pString = LitString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

-- Literal = String | Float | Integer | Boolean
-- Obs: Float vem antes de Integer para o parser tentar ler o ponto flutuante primeiro
pLiteral :: Parser Expr
pLiteral = 
        pString
    <|> try pFloat
    <|> try pInteger
    <|> try pBoolean

--------------------------------------------------------------------------------
-- 3. IDENTIFICADORES E TIPOS
--------------------------------------------------------------------------------

pIdentifier :: Parser String
pIdentifier = lexeme $ do
    first <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    return (first : rest)

pType :: Parser String
pType = lexeme $ 
        (string "Int") 
    <|> (string "Float") 
    <|> (string "String") 
    <|> (string "Bool")
    <|> do
        symbol "["
        t <- pType
        symbol "]"
        return ("[" ++ t ++ "]")

--------------------------------------------------------------------------------
-- 4. EXPRESSÕES E PRECEDÊNCIA
--------------------------------------------------------------------------------

pAtom :: Parser Expr
pAtom = 
        try pLiteral
    <|> pList
    <|> (Var <$> pIdentifier)
    <|> between (symbol "(") (symbol ")") pExpression

pList :: Parser Expr
pList = do
    symbol "["
    elements <- pExpression `sepBy` symbol ","
    symbol "]"
    return (ListExpr elements)

pConcatOperator :: Parser String
pConcatOperator = lexeme $ do
    _ <- char ':'
    notFollowedBy (char ':')
    sc
    return ":"

pPlusOperator :: Parser String
pPlusOperator = try $ lexeme $ do
    _ <- char '+'
    notFollowedBy (char '+')
    return "+"

pLet :: Parser Expr
pLet = do
    symbol "let"
    assignments <- pAssignment `sepBy1` symbol ";"
    symbol "in"
    expr <- pExpression
    return (Let assignments expr)

-- CallFunction = Identifier, Atom, { Atom }
pCallFunction :: Parser Expr
pCallFunction = do
    name <- pIdentifier
    symbol "("
    -- Lê 1 ou mais átomos como argumentos da função
    args <- pExpression `sepBy1` symbol ","
    symbol ")"
    return (Call name args)

pReturnExpr :: Parser Expr
pReturnExpr = 
        try pLet
    <|> try pCallFunction 
    <|> pAtom

-- Tabela de operadores usando makeExprParser do Megaparsec para substituir a cascata manual
pExpression :: Parser Expr
pExpression = makeExprParser pReturnExpr operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixR (BinaryOp "^" <$ symbol "^") ] -- Exponenciação é associativa à direita, então usamos InfixR 2 ^ 3 != 3 ^ 2 
  , [ InfixL (BinaryOp "*" <$ symbol "*")
    , InfixL (BinaryOp "/" <$ symbol "/")
    , InfixL (BinaryOp "%" <$ symbol "%") ]
  , [ InfixL (BinaryOp "+" <$ pPlusOperator)
    , InfixL (BinaryOp "-" <$ symbol "-") ]
  , [ InfixR (BinaryOp ":" <$ pConcatOperator) ]
  , [ InfixR (BinaryOp "++" <$ symbol "++") ]
  , [ InfixL (BinaryOp ">=" <$ symbol ">=")
    , InfixL (BinaryOp "<=" <$ symbol "<=")
    , InfixL (BinaryOp ">"  <$ symbol ">")
    , InfixL (BinaryOp "<"  <$ symbol "<") ]
  , [ InfixL (BinaryOp "==" <$ symbol "==")
    , InfixL (BinaryOp "!=" <$ symbol "!=") ]
  , [ InfixL (BinaryOp "&&" <$ symbol "&&") ]
  , [ InfixL (BinaryOp "||" <$ symbol "||") ]
  ]

--------------------------------------------------------------------------------
-- 5. DECLARAÇÕES E PROGRAMA
--------------------------------------------------------------------------------

-- FuncAnnotation = Identifier, "::", Type, { "->", Type }
pFuncAnnotation :: Parser Declaration
pFuncAnnotation = do
    name <- pIdentifier
    symbol "::"
    t1 <- pType
    ts <- many (symbol "->" *> pType)
    return (FuncAnnot name (t1 : ts))

-- FuncDef = Identifier, { Identifier }, "=", Expression
pFuncDef :: Parser Declaration
pFuncDef = do
    name <- pIdentifier
    params <- some pIdentifier
    symbol "="
    body <- pExpression
    return (FuncDef name params body)

-- Assignment = Identifier, AssignOperators, Expression
pAssignment :: Parser Declaration
pAssignment = do
    name <- pIdentifier
    op <- symbol "=" <|> symbol "+=" <|> symbol "-=" <|> symbol "*=" <|> symbol "/="
    expr <- pExpression
    return (Assignment name op expr)

pStatement :: Parser Statement
pStatement = 
        try (DeclarationStatement <$> pFuncAnnotation)
    <|> try (ExprStatement <$> pLet)
    <|> try (DeclarationStatement <$> pFuncDef)
    <|> try (DeclarationStatement <$> pAssignment)
    <|> (ExprStatement <$> pExpression)

pProgram :: Parser Program
pProgram = do
    sc
    statements <- pStatement `sepEndBy` newline
    sc
    eof
    return statements
