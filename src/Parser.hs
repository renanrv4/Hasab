module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad.Combinators.Expr

-- Tipo base do Parser
type Parser = Parsec Void String

--------------------------------------------------------------------------------
-- 1. CONSUMIDOR DE ESPAÇOS E LEXEMAS
--------------------------------------------------------------------------------

-- Ignora espaços, tabulações, quebras de linha e comentários
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

-- Garante que após ler um token, os espaços à frente sejam consumidos
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Lê uma string exata ignorando espaços ao redor
symbol :: String -> Parser String
symbol = L.symbol sc

--------------------------------------------------------------------------------
-- 2. TIPOS BÁSICOS E LITERAIS
--------------------------------------------------------------------------------

pInteger :: Parser Integer
pInteger = lexeme (L.signed sc L.decimal)

pFloat :: Parser Double
pFloat = lexeme (L.signed sc L.float)

pBoolean :: Parser Bool
pBoolean = lexeme ((True <$ string "true") <|> (False <$ string "false"))

pString :: Parser String
pString = lexeme (char '"' *> manyTill L.charLiteral (char '"'))

-- Literal = String | Float | Integer | Boolean
-- Nota: Float vem antes de Integer para o parser tentar ler o ponto flutuante primeiro
pLiteral :: Parser AST
pLiteral = 
        (LitString <$> pString)
    <|> try (LitFloat <$> pFloat)
    <|> (LitInt <$> pInteger)
    <|> (LitBool <$> pBoolean)

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
-- 4. ESTRUTURA DE AST (Árvore Sintática Abstrata de Apoio)
--------------------------------------------------------------------------------

data AST 
    = LitInt Integer
    | LitFloat Double
    | LitBool Bool
    | LitString String
    | Var String
    | ListExpr [AST]
    | Call String [AST]
    | BinaryOp String AST AST
    | Assignment String String AST
    | FuncAnnot String [String]
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 5. EXPRESSÕES E PRECEDÊNCIA (Baseado na sua cascata EBNF)
--------------------------------------------------------------------------------

pAtom :: Parser AST
pAtom = 
        try pLiteral
    <|> pList
    <|> (Var <$> pIdentifier)
    <|> between (symbol "(") (symbol ")") pExpression

pList :: Parser AST
pList = do
    symbol "["
    elements <- pExpression `sepBy` symbol ","
    symbol "]"
    return (ListExpr elements)

-- CallFunction = Identifier, Atom, { Atom }
pCallFunction :: Parser AST
pCallFunction = do
    name <- pIdentifier
    -- Lê 1 ou mais átomos como argumentos da função
    args <- some pAtom
    return (Call name args)

pReturnExpr :: Parser AST
pReturnExpr = try pCallFunction <|> pAtom

-- Tabela de operadores usando makeExprParser do Megaparsec para substituir a cascata manual
pExpression :: Parser AST
pExpression = makeExprParser pReturnExpr operatorTable

operatorTable :: [[Operator Parser AST]]
operatorTable =
  [ [ InfixL (BinaryOp "*" <$ symbol "*")
    , InfixL (BinaryOp "/" <$ symbol "/")
    , InfixL (BinaryOp "%" <$ symbol "%") ]
  , [ InfixL (BinaryOp "+" <$ symbol "+")
    , InfixL (BinaryOp "-" <$ symbol "-") ]
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
-- 6. DECLARAÇÕES E PROGRAMA
--------------------------------------------------------------------------------

-- FuncAnnotation = Identifier, "::", Type, { "->", Type }
pFuncAnnotation :: Parser AST
pFuncAnnotation = do
    name <- pIdentifier
    symbol "::"
    t1 <- pType
    ts <- many (symbol "->" *> pType)
    return (FuncAnnot name (t1 : ts))

-- Assignment = Identifier, AssignOperators, Expression
pAssignment :: Parser AST
pAssignment = do
    name <- pIdentifier
    op <- symbol "=" <|> symbol "+=" <|> symbol "-=" <|> symbol "*=" <|> symbol "/="
    expr <- pExpression
    return (Assignment name op expr)

pStatement :: Parser AST
pStatement = 
        try pFuncAnnotation
    <|> try pAssignment
    <|> pExpression

pProgram :: Parser [AST]
pProgram = sc *> many pStatement <* eof
