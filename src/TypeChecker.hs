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
-- 2. FUNÇÃO PRINCIPAL DE INFERÊNCIA/CHECAGEM (Para Expressões)
--------------------------------------------------------------------------------

-- | Recebe o ambiente atual e uma expressão. Retorna um Erro (String) ou o Tipo final.
typeCheckExpr :: TypeEnv -> Expr -> Either String Type
typeCheckExpr env expr = case expr of

    -- Literais são triviais: o tipo é direto
    LitInt _    -> Right TInt
    LitFloat _  -> Right TFloat
    LitBool _   -> Right TBool
    LitString _ -> Right TString

    -- Variáveis: buscamos o nome no ambiente (Map)
    Var name -> case Map.lookup name env of
        Just t  -> Right t
        Nothing -> Left $ "Variável não declarada: " ++ name

    -- Listas: validamos se todos os elementos têm o mesmo tipo
    ListExpr elements -> do
        if null elements
            then Left "Não é possível inferir o tipo de uma lista vazia"
            else do
                -- Checa o tipo de todos os elementos
                types <- mapM (typeCheckExpr env) elements
                let firstType = head types
                -- Verifica se todos são iguais ao primeiro
                if all (== firstType) types
                    then Right (TList firstType)
                    else Left "Lista heterogênea: todos os elementos devem ter o mesmo tipo"

    -- Condicional: 'if' precisa de um Bool, e 'then' e 'else' precisam ter o mesmo tipo
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

    -- Operações Binárias: validamos operador por operador
    BinaryOp op e1 e2 -> do
        t1 <- typeCheckExpr env e1
        t2 <- typeCheckExpr env e2
        typeCheckBinaryOp op t1 t2

    -- Para Let e Call, a lógica é um pouco mais extensa. 
    -- Faremos um mock simples para chamadas de função aqui.
    Call name args -> case Map.lookup name env of
        Just (TFunc paramTypes returnType) -> do
            argTypes <- mapM (typeCheckExpr env) args
            if argTypes == paramTypes
                then Right returnType
                else Left $ "Argumentos inválidos para a função '" ++ name ++ "'"
        Just _  -> Left $ "'" ++ name ++ "' não é uma função"
        Nothing -> Left $ "Função não declarada: " ++ name

    -- O 'Let' precisaria expandir o 'env' temporariamente com as variáveis declaradas
    Let _ _ -> Left "Checagem de tipo para 'Let' ainda não implementada"

--------------------------------------------------------------------------------
-- 3. REGRAS PARA OPERADORES BINÁRIOS
--------------------------------------------------------------------------------

typeCheckBinaryOp :: String -> Type -> Type -> Either String Type
typeCheckBinaryOp op t1 t2
    -- Operadores Aritméticos (+, -, *, /)
    | op `elem` ["+", "-", "*", "/"] =
        if (t1 == TInt || t1 == TFloat) && t1 == t2
            then Right t1
            else Left $ "Operador aritmético '" ++ op ++ "' incompatível com: " ++ show t1 ++ " e " ++ show t2

    -- Operadores Lógicos (&&, ||)
    | op `elem` ["&&", "||"] =
        if t1 == TBool && t2 == TBool
            then Right TBool
            else Left $ "Operador lógico '" ++ op ++ "' espera Bools. Recebeu: " ++ show t1 ++ " e " ++ show t2

    -- Operadores Relacionais (==, !=, <, >)
    | op `elem` ["==", "!=", "<", ">", "<=", ">="] =
        if t1 == t2 
            then Right TBool  -- Comparações sempre retornam Bool
            else Left $ "Incompatibilidade no operador relacional '" ++ op ++ "': " ++ show t1 ++ " e " ++ show t2

    -- Concatenação de Listas (++)
    | op == "++" = case (t1, t2) of
        (TList elemT1, TList elemT2) -> 
            if elemT1 == elemT2
                then Right t1
                else Left "Não é possível concatenar listas de tipos diferentes"
        _ -> Left "O operador '++' só pode ser usado em listas"

    | otherwise = Left $ "Operador desconhecido: " ++ op