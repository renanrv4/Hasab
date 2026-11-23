module Parser where

import AST
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Tipo base do Parser
type Parser = Parsec Void String

--------------------------------------------------------------------------------
-- 1. CONSUMIDORES DE ESPAÇO E LEXEMAS (SEPARAÇÃO HORIZONTAL vs QUEBRA DE LINHA)
--------------------------------------------------------------------------------

-- | Consome apenas espaços horizontais (espaço/tab) e comentários na mesma linha.
-- NÃO consome quebras de linha (\n), evitando que expressões invadam a linha seguinte.
sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | Consome todos os espaços, incluindo quebras de linha (\n) e comentários.
-- Usado entre instruções do programa e dentro de blocos delimitadores.
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | Lê um token e consome espaços horizontais à direita.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Lê um símbolo exato e consome espaços horizontais à direita.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Separador de instruções: aceita ';', '\n' ou combinações (ex: ';\n'),
-- seguido de eventuais linhas em branco ou comentários.
stmtSep :: Parser ()
stmtSep = some (void (symbol ";") <|> void eol) *> scn

--------------------------------------------------------------------------------
-- 2. PALAVRAS RESERVADAS E LITERAIS
--------------------------------------------------------------------------------

reservedWords :: [String]
reservedWords =
    [ "let", "in", "if", "then", "else"
    , "true", "false"
    , "Int", "Float", "String", "Bool"
    ]

-- | Literais inteiros sem sinal (o sinal '-' é tratado como operador unário Neg)
pInteger :: Parser Expr
pInteger = lexeme $ do
    n <- L.decimal
    notFollowedBy (alphaNumChar <|> char '_')
    return (LitInt n)

-- | Literais de ponto flutuante sem sinal
pFloat :: Parser Expr
pFloat = lexeme $ do
    n <- L.float
    notFollowedBy (alphaNumChar <|> char '_')
    return (LitFloat n)

pBoolean :: Parser Expr
pBoolean = lexeme $ do
    value <- (True <$ string "true") <|> (False <$ string "false")
    notFollowedBy (alphaNumChar <|> char '_')
    return (LitBool value)

pString :: Parser Expr
pString = LitString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pLiteral :: Parser Expr
pLiteral =
        pString
    <|> try pFloat
    <|> pInteger
    <|> pBoolean

--------------------------------------------------------------------------------
-- 3. IDENTIFICADORES E TIPOS ESTRUTURADOS
--------------------------------------------------------------------------------

pIdentifier :: Parser String
pIdentifier = lexeme $ try $ do
    first <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    let name = first : rest
    if name `elem` reservedWords
        then fail $ "palavra reservada '" ++ name ++ "' não pode ser usada como identificador"
        else return name

pType :: Parser TypeExpr
pType = lexeme $
        (TyInt    <$ string "Int")
    <|> (TyFloat  <$ string "Float")
    <|> (TyString <$ string "String")
    <|> (TyBool   <$ string "Bool")
    <|> between (symbol "[") (symbol "]") (TyList <$> pType)

--------------------------------------------------------------------------------
-- 4. EXPRESSÕES, OPERADORES E PRECEDÊNCIA
--------------------------------------------------------------------------------

pAtom :: Parser Expr
pAtom =
        try pLiteral
    <|> pEmptyList
    <|> pList
    <|> (Var <$> pIdentifier)
    <|> between (symbol "(" *> scn) (scn *> symbol ")") pExpression

-- | Reconhece a lista vazia '[]' (incluindo espaços ou quebras de linha internas)
pEmptyList :: Parser Expr
pEmptyList = EmptyList <$ try (symbol "[" *> scn <* symbol "]")

-- | Reconhece listas literais não-vazias '[e1, e2, ...]' (garante >= 1 elemento)
pList :: Parser Expr
pList = do
    _ <- symbol "["
    _ <- scn
    elements <- (pExpression <* scn) `sepBy1` (symbol "," <* scn)
    _ <- symbol "]"
    return (ListExpr elements)

-- | Sufixo de indexação de listas: xs[i] ou matriz[i][j]
pIndexSuffix :: Parser (Expr -> Expr)
pIndexSuffix = do
    _ <- symbol "["
    _ <- scn
    idx <- pExpression
    _ <- scn
    _ <- symbol "]"
    return (`Index` idx)

pNotOperator :: Parser UnaryOp
pNotOperator = try $ lexeme $ do
    _ <- char '!'
    notFollowedBy (char '=')
    return Not

pConcatOperator :: Parser BinOp
pConcatOperator = try $ lexeme $ do
    _ <- char ':'
    notFollowedBy (char ':')
    return Cons

pPlusOperator :: Parser BinOp
pPlusOperator = try $ lexeme $ do
    _ <- char '+'
    notFollowedBy (char '+')
    return Add

-- | Bloco Let aceita atribuições e funções locais, separadas por ';' ou '\n'
pLet :: Parser Expr
pLet = do
    _ <- symbol "let"
    _ <- scn
    decls <- pLocalDeclaration `sepEndBy1` stmtSep
    _ <- symbol "in"
    _ <- scn
    expr <- pExpression
    return (Let decls expr)

pIfThenElse :: Parser Expr
pIfThenElse = do
    _ <- symbol "if"
    _ <- scn
    cond <- pExpression
    _ <- scn
    _ <- symbol "then"
    _ <- scn
    thenExpr <- pExpression
    _ <- scn
    _ <- symbol "else"
    _ <- scn
    elseExpr <- pExpression
    return (IfThenElse cond thenExpr elseExpr)

-- | Chamada de função aceita 0 ou mais argumentos (sepBy)
pCallFunction :: Parser Expr
pCallFunction = do
    name <- pIdentifier
    _ <- symbol "("
    _ <- scn
    args <- (pExpression <* scn) `sepBy` (symbol "," <* scn)
    _ <- symbol ")"
    return (Call name args)

pReturnExpr :: Parser Expr
pReturnExpr =
        pLet
    <|> pIfThenElse
    <|> try pCallFunction
    <|> pAtom

pExpression :: Parser Expr
pExpression = makeExprParser pReturnExpr operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ Postfix (foldr1 (.) . reverse <$> some pIndexSuffix) ]
    , [ Prefix  (UnaryOp Neg <$ symbol "-")
      , Prefix  (UnaryOp <$> pNotOperator) ]
    , [ InfixR  (BinaryOp Pow <$ symbol "^") ]
    , [ InfixL  (BinaryOp Mul <$ symbol "*")
      , InfixL  (BinaryOp Div <$ symbol "/")
      , InfixL  (BinaryOp Mod <$ symbol "%") ]
    , [ InfixL  (BinaryOp Add <$ pPlusOperator)
      , InfixL  (BinaryOp Sub <$ symbol "-") ]
    , [ InfixR  (BinaryOp <$> pConcatOperator) ]
    , [ InfixR  (BinaryOp Concat <$ symbol "++") ]
    , [ InfixL  (BinaryOp Geq <$ symbol ">=")
      , InfixL  (BinaryOp Leq <$ symbol "<=")
      , InfixL  (BinaryOp Gt  <$ symbol ">")
      , InfixL  (BinaryOp Lt  <$ symbol "<") ]
    , [ InfixL  (BinaryOp Eq  <$ symbol "==")
      , InfixL  (BinaryOp Neq <$ symbol "!=") ]
    , [ InfixL  (BinaryOp And <$ symbol "&&") ]
    , [ InfixL  (BinaryOp Or  <$ symbol "||") ]
    ]

--------------------------------------------------------------------------------
-- 5. DECLARAÇÕES E PROGRAMA
--------------------------------------------------------------------------------

pAssignOp :: Parser AssignOp
pAssignOp =
        (AddAssign <$ symbol "+=")
    <|> (SubAssign <$ symbol "-=")
    <|> (MulAssign <$ symbol "*=")
    <|> (DivAssign <$ symbol "/=")
    <|> (Assign    <$ symbol "=")

-- | Backtracking restrito apenas ao cabeçalho "nome ::"
pFuncAnnotation :: Parser Declaration
pFuncAnnotation = do
    name <- try (pIdentifier <* symbol "::")
    t1 <- pType
    ts <- many (symbol "->" *> pType)
    return (FuncAnnot name (t1 : ts))

-- | Backtracking restrito apenas ao cabeçalho "nome p1 p2 ="
pFuncDef :: Parser Declaration
pFuncDef = do
    (name, params) <- try ((,) <$> pIdentifier <*> some pIdentifier <* symbol "=")
    _ <- scn
    body <- pExpression
    return (FuncDef name params body)

-- | Backtracking restrito apenas ao cabeçalho "nome op="
pAssignment :: Parser Declaration
pAssignment = do
    (name, op) <- try ((,) <$> pIdentifier <*> pAssignOp)
    _ <- scn
    expr <- pExpression
    return (Assignment name op expr)

pLocalDeclaration :: Parser Declaration
pLocalDeclaration =
        pFuncAnnotation
    <|> pFuncDef
    <|> pAssignment

pStatement :: Parser Statement
pStatement =
        (DeclarationStatement <$> pLocalDeclaration)
    <|> (ExprStatement <$> pExpression)

pProgram :: Parser Program
pProgram = scn *> (pStatement `sepEndBy` stmtSep) <* eof