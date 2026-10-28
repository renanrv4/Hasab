module TypeChecker where

import AST
import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- 1. DEFINIÇÃO DOS TIPOS DA LINGUAGEM
--------------------------------------------------------------------------------

data Type
    = TInt
    | TFloat
    | TBool
    | TString
    | TList Type           -- Ex: TList TInt para [Int]
    | TFunc [Type] Type    -- Ex: TFunc [TInt, TInt] TInt para Int -> Int -> Int
    deriving (Show, Eq)

-- O Ambiente de Tipos (Environment) armazena o tipo associado a cada variável
type TypeEnv = Map String Type

--------------------------------------------------------------------------------
-- 2. FUNÇÃO PRINCIPAL DE INFERÊNCIA/CHECAGEM
--------------------------------------------------------------------------------

typeCheckExpr :: TypeEnv -> Expr -> Either String Type
typeCheckExpr env expr = case expr of

    LitInt _    -> Right TInt
    LitFloat _  -> Right TFloat
    LitBool _   -> Right TBool
    LitString _ -> Right TString

    Var name -> case Map.lookup name env of
        Just t  -> Right t
        Nothing -> Left $ "Variável não declarada: " ++ name

    ListExpr elements -> do
        if null elements
            then Left "Não é possível inferir o tipo de uma lista vazia"
            else do
                types <- mapM (typeCheckExpr env) elements
                let firstType = head types
                if all (== firstType) types
                    then Right (TList firstType)
                    else Left "Lista heterogênea: todos os elementos devem ter o mesmo tipo"

    IfThenElse cond thenExpr elseExpr -> do
        tCond <- typeCheckExpr env cond
        if tCond /= TBool
            then Left $ "A condição do 'if' deve ser Bool, mas recebeu: " ++ show tCond
            else do
                tThen <- typeCheckExpr env thenExpr
                tElse <- typeCheckExpr env elseExpr
                if tThen == tElse
                    then Right tThen
                    else Left $ "Os ramos 'then' e 'else' devem ter o mesmo tipo. Recebeu: " ++ show tThen ++ " e " ++ show tElse

    BinaryOp op e1 e2 -> do
        t1 <- typeCheckExpr env e1
        t2 <- typeCheckExpr env e2
        typeCheckBinaryOp op t1 t2

    Call name args -> case Map.lookup name env of
        Just (TFunc paramTypes returnType) -> do
            argTypes <- mapM (typeCheckExpr env) args
            if argTypes == paramTypes
                then Right returnType
                else Left $ "Argumentos inválidos para a função '" ++ name ++ "'"
        Just _  -> Left $ "'" ++ name ++ "' não é uma função"
        Nothing -> Left $ "Função não declarada: " ++ name

    Let _ _ -> Left "Checagem de tipo para 'Let' ainda não implementada"

--------------------------------------------------------------------------------
-- 3. REGRAS PARA OPERADORES BINÁRIOS (ATUALIZADO)
--------------------------------------------------------------------------------

typeCheckBinaryOp :: String -> Type -> Type -> Either String Type
typeCheckBinaryOp op t1 t2
    -- Operadores Aritméticos (+, -, *, /, ^, %)
    -- Inclui exponenciação (^) e resto/módulo (%)
    | op `elem` ["+", "-", "*", "/", "^", "%"] =
        if (t1 == TInt || t1 == TFloat) && t1 == t2
            then Right t1
            else Left $ "Operador aritmético '" ++ op ++ "' incompatível com: " ++ show t1 ++ " e " ++ show t2

    -- Operador Cons (:) -> Adiciona um elemento no início da lista (ex: 1 : [2, 3])
    | op == ":" = case t2 of
        TList elemT ->
            if t1 == elemT
                then Right (TList elemT)
                else Left $ "Incompatibilidade no operador ':': não é possível adicionar um " 
                         ++ show t1 ++ " em uma lista de " ++ show elemT
        _ -> Left $ "O lado direito do operador ':' deve ser uma lista. Recebeu: " ++ show t2

    -- Concatenação de Listas (++)
    | op == "++" = case (t1, t2) of
        (TList elemT1, TList elemT2) -> 
            if elemT1 == elemT2
                then Right t1
                else Left "Não é possível concatenar listas de tipos diferentes"
        _ -> Left "O operador '++' só pode ser usado em listas"

    -- Operadores Lógicos (&&, ||)
    | op `elem` ["&&", "||"] =
        if t1 == TBool && t2 == TBool
            then Right TBool
            else Left $ "Operador lógico '" ++ op ++ "' espera Bools. Recebeu: " ++ show t1 ++ " e " ++ show t2

    -- Operadores Relacionais (==, !=, <, >, <=, >=)
    | op `elem` ["==", "!=", "<", ">", "<=", ">="] =
        if t1 == t2 
            then Right TBool
            else Left $ "Incompatibilidade no operador relacional '" ++ op ++ "': " ++ show t1 ++ " e " ++ show t2

    | otherwise = Left $ "Operador desconhecido: " ++ op