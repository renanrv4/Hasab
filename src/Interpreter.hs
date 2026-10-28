module Interpreter where

import AST
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

--------------------------------------------------------------------------------
-- 1. ESTRUTURAS DE VALORES EM TEMPO DE EXECUÇÃO (RUNTIME)
--------------------------------------------------------------------------------

-- | Representa os valores finais calculados na memória
data Value
    = VInt Integer
    | VFloat Double
    | VBool Bool
    | VString String
    | VList [Value]
    | VFunction [String] Expr RuntimeEnv  -- Clausura: Parâmetros, Corpo e Escopo
    | VUnit                               -- Retorno para declarações/atribuições
    deriving (Show, Eq)

-- | O Ambiente de Execução mapeia nomes de variáveis/funções para seus Valores
type RuntimeEnv = Map String Value

--------------------------------------------------------------------------------
-- 2. AVALIAÇÃO DE EXPRESSÕES (evalExpr)
--------------------------------------------------------------------------------

evalExpr :: RuntimeEnv -> Expr -> Either String Value
evalExpr env expr = case expr of

    -- Literais
    LitInt n    -> Right (VInt n)
    LitFloat f  -> Right (VFloat f)
    LitBool b   -> Right (VBool b)
    LitString s -> Right (VString s)

    -- Variáveis
    Var name -> case Map.lookup name env of
        Just val -> Right val
        Nothing  -> Left $ "Erro de execução: Variável '" ++ name ++ "' não encontrada."

    -- Listas
    ListExpr elems -> do
        vals <- mapM (evalExpr env) elems
        Right (VList vals)

    -- Condicional (IfThenElse)
    IfThenElse cond thenExpr elseExpr -> do
        condVal <- evalExpr env cond
        case condVal of
            VBool True  -> evalExpr env thenExpr
            VBool False -> evalExpr env elseExpr
            _           -> Left "Erro de execução: A condição do 'if' deve resultar em Booleano."

    -- Bloco Let (Cria um escopo local estendido)
    Let decls body -> do
        localEnv <- foldM evalLetDecl env decls
        evalExpr localEnv body

    -- Chamada de Função
    Call name args -> case Map.lookup name env of
        Just (VFunction params body closureEnv) -> do
            argVals <- mapM (evalExpr env) args
            if length params /= length argVals
                then Left $ "Erro na função '" ++ name ++ "': esperava " 
                         ++ show (length params) ++ " argumentos, mas recebeu " 
                         ++ show (length argVals)
                else do
                    -- Associa parâmetros aos valores reais e combina com a clausura e ambiente atual
                    let paramBindings = Map.fromList (zip params argVals)
                    let localEnv = Map.union paramBindings (Map.union env closureEnv)
                    evalExpr localEnv body

        Just _  -> Left $ "Erro de execução: '" ++ name ++ "' não é uma função."
        Nothing -> Left $ "Erro de execução: Função '" ++ name ++ "' não declarada."

    -- Operadores Binários
    BinaryOp op e1 e2 -> do
        v1 <- evalExpr env e1
        v2 <- evalExpr env e2
        evalBinaryOp op v1 v2

--------------------------------------------------------------------------------
-- 3. AVALIAÇÃO DE OPERADORES BINÁRIOS (TODOS OS 16 OPERADORES)
--------------------------------------------------------------------------------

evalBinaryOp :: String -> Value -> Value -> Either String Value
evalBinaryOp op v1 v2 = case (op, v1, v2) of

    -- Aritmética básica
    ("+", VInt a, VInt b)     -> Right $ VInt (a + b)
    ("+", VFloat a, VFloat b) -> Right $ VFloat (a + b)
    ("-", VInt a, VInt b)     -> Right $ VInt (a - b)
    ("-", VFloat a, VFloat b) -> Right $ VFloat (a - b)
    ("*", VInt a, VInt b)     -> Right $ VInt (a * b)
    ("*", VFloat a, VFloat b) -> Right $ VFloat (a * b)
    ("/", VInt _, VInt 0)     -> Left "Erro de execução: Divisão por zero."
    ("/", VInt a, VInt b)     -> Right $ VInt (a `div` b)
    ("/", VFloat _, VFloat 0) -> Left "Erro de execução: Divisão por zero."
    ("/", VFloat a, VFloat b) -> Right $ VFloat (a / b)

    -- Exponenciação (^)
    ("^", VInt a, VInt b)     -> Right $ VInt (a ^ b)
    ("^", VFloat a, VFloat b) -> Right $ VFloat (a ** b)

    -- Módulo (%)
    ("%", VInt _, VInt 0)     -> Left "Erro de execução: Módulo por zero."
    ("%", VInt a, VInt b)     -> Right $ VInt (a `mod` b)

    -- Operações com Listas (Cons : e Concatenação ++)
    (":", elemVal, VList lst)  -> Right $ VList (elemVal : lst)
    ("++", VList l1, VList l2) -> Right $ VList (l1 ++ l2)

    -- Operadores Lógicos (&&, ||)
    ("&&", VBool a, VBool b)   -> Right $ VBool (a && b)
    ("||", VBool a, VBool b)   -> Right $ VBool (a || b)

    -- Operadores de Relacionais/Comparação
    ("==", a, b)              -> Right $ VBool (a == b)
    ("!=", a, b)              -> Right $ VBool (a /= b)
    ("<",  VInt a, VInt b)     -> Right $ VBool (a < b)
    ("<",  VFloat a, VFloat b) -> Right $ VBool (a < b)
    (">",  VInt a, VInt b)     -> Right $ VBool (a > b)
    (">",  VFloat a, VFloat b) -> Right $ VBool (a > b)
    ("<=", VInt a, VInt b)     -> Right $ VBool (a <= b)
    ("<=", VFloat a, VFloat b) -> Right $ VBool (a <= b)
    (">=", VInt a, VInt b)     -> Right $ VBool (a >= b)
    (">=", VFloat a, VFloat b) -> Right $ VBool (a >= b)

    _ -> Left $ "Erro de execução: Operador '" ++ op ++ "' incompatível com os tipos: " 
             ++ show v1 ++ " e " ++ show v2

--------------------------------------------------------------------------------
-- 4. AVALIAÇÃO DE DECLARAÇÕES E ATRIBUIÇÕES LOCAIS (LET)
--------------------------------------------------------------------------------

evalLetDecl :: RuntimeEnv -> Declaration -> Either String RuntimeEnv
evalLetDecl env decl = case decl of
    Assignment name op expr -> do
        val <- evalExpr env expr
        finalVal <- case op of
            "="  -> Right val
            "+=" -> applyCompoundOp "+" name val env
            "-=" -> applyCompoundOp "-" name val env
            "*=" -> applyCompoundOp "*" name val env
            "/=" -> applyCompoundOp "/" name val env
            _    -> Left $ "Operador de atribuição desconhecido: " ++ op
        Right (Map.insert name finalVal env)
    
    FuncDef name params body ->
        Right (Map.insert name (VFunction params body env) env)
        
    FuncAnnot _ _ -> Right env

applyCompoundOp :: String -> String -> Value -> RuntimeEnv -> Either String Value
applyCompoundOp op name newVal env = case Map.lookup name env of
    Just oldVal -> evalBinaryOp op oldVal newVal
    Nothing     -> Left $ "Erro: Variável '" ++ name ++ "' não declarada para operador composto."

--------------------------------------------------------------------------------
-- 5. AVALIAÇÃO DE INSTRUÇÕES E PROGRAMAS MULTILINHA
--------------------------------------------------------------------------------

-- | Avalia uma instrução (Statement) e retorna o valor + ambiente atualizado
evalStatement :: RuntimeEnv -> Statement -> Either String (Value, RuntimeEnv)
evalStatement env stmt = case stmt of

    ExprStatement expr -> do
        val <- evalExpr env expr
        Right (val, env)

    DeclarationStatement decl -> case decl of
        Assignment name op expr -> do
            val <- evalExpr env expr
            finalVal <- case op of
                "="  -> Right val
                "+=" -> applyCompoundOp "+" name val env
                "-=" -> applyCompoundOp "-" name val env
                "*=" -> applyCompoundOp "*" name val env
                "/=" -> applyCompoundOp "/" name val env
                _    -> Left $ "Operador de atribuição desconhecido: " ++ op
            Right (VUnit, Map.insert name finalVal env)

        FuncDef name params body ->
            let closure = VFunction params body env
            in Right (VUnit, Map.insert name closure env)

        FuncAnnot _ _ -> Right (VUnit, env)

-- | Avalia uma sequência de instruções acumulando o ambiente de memória
evalProgram :: RuntimeEnv -> Program -> Either String (Value, RuntimeEnv)
evalProgram env [] = Right (VUnit, env)
evalProgram env [lastStmt] = evalStatement env lastStmt
evalProgram env (stmt:stmts) = do
    (_, newEnv) <- evalStatement env stmt
    evalProgram newEnv stmts