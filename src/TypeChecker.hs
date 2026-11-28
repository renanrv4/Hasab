module TypeChecker where

import AST
import Control.Monad (foldM, zipWithM)
import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- 1. DEFINIÇÃO DOS TIPOS ESTÁTICOS E PRIMITIVAS
--------------------------------------------------------------------------------

data Type
    = TInt
    | TFloat
    | TBool
    | TString
    | TEmptyList               -- Tipo polimórfico de lista vazia: []
    | TList Type               -- Lista tipada homogênea: [T]
    | TFunc [Type] Type        -- Assinatura de função: [ParamTypes] -> ReturnType
--    | TBuiltin BuiltinOp       -- Primitivas polimórficas (head, tail, len)
    | TUnit                    -- Tipo de instruções que apenas alteram ambiente
    deriving (Show, Eq)

type TypeEnv = Map String Type

-- | Ambiente inicial com funções builtin
initialTypeEnv :: TypeEnv
initialTypeEnv = Map.fromList
    []

-- | Converte a anotação sintática da AST (TypeExpr) para o tipo semântico (Type)
typeExprToType :: TypeExpr -> Type
typeExprToType TyInt       = TInt
typeExprToType TyFloat     = TFloat
typeExprToType TyBool      = TBool
typeExprToType TyString    = TString
typeExprToType (TyList te) = TList (typeExprToType te)

--------------------------------------------------------------------------------
-- 2. SUPORTE À LISTA VAZIA [])
--------------------------------------------------------------------------------

-- | Unifica dois tipos compatíveis
unifyTypes :: Type -> Type -> Either String Type
-- | Unifica TEmptyList com outra Lista
unifyTypes TEmptyList (TList t)  = Right (TList t)
unifyTypes (TList t) TEmptyList  = Right (TList t)
unifyTypes TEmptyList TEmptyList = Right TEmptyList

unifyTypes (TList a) (TList b)   = TList <$> unifyTypes a b

unifyTypes (TFunc p1 r1) (TFunc p2 r2)
    | length p1 == length p2 = TFunc <$> zipWithM unifyTypes p1 p2 <*> unifyTypes r1 r2
    | otherwise = Left "Aridades de função incompatíveis."

unifyTypes t1 t2
    | t1 == t2  = Right t1 -- tipos compatíveis
    | otherwise = Left $ "Incompatibilidade de tipos: esperado " ++ show t1 ++ ", mas encontrou " ++ show t2

-- | Verifica se um tipo contém funções ou Unit (não comparáveis com == / !=)
-- | talvez seja melhor deixar explicito quem pode
isNonComparable :: Type -> Bool
isNonComparable (TFunc _ _)  = True
-- isNonComparable (TBuiltin _) = True
isNonComparable TUnit        = True
isNonComparable (TList t)    = isNonComparable t
isNonComparable _            = False

--------------------------------------------------------------------------------
-- 3. CHECAGEM DE EXPRESSÕES (typeCheckExpr)
--------------------------------------------------------------------------------

typeCheckExpr :: TypeEnv -> Expr -> Either String Type
typeCheckExpr env expr = case expr of

    -- Literais e Lista Vazia
    LitInt _    -> Right TInt
    LitFloat _  -> Right TFloat
    LitBool _   -> Right TBool
    LitString _ -> Right TString
    EmptyList   -> Right TEmptyList

    -- Variáveis
    Var name -> case Map.lookup name env of -- verifica se a variável foi declarada
        Just t  -> Right t
        Nothing -> Left $ "Erro de tipo: variável não declarada " ++ name

    -- Listas não-vazias (unifica todos os elementos, inclusive listas aninhadas com [])
    ListExpr [] -> Right TEmptyList 
    ListExpr (e:es) -> do
        firstT <- typeCheckExpr env e
        elemT  <- foldM (\acc x -> typeCheckExpr env x >>= unifyTypes acc) firstT es -- checa se todos os elementos têm o mesmo tipo
        Right (TList elemT)

    -- Indexação de listas: xs[i]
    Index listExpr idxExpr -> do
        tList <- typeCheckExpr env listExpr
        tIdx  <- typeCheckExpr env idxExpr
        if tIdx /= TInt
            then Left $ "Erro de tipo: index tem que ser Int, o tipo do index é " ++ show tIdx
            else case tList of
                TList elemT -> Right elemT
                TEmptyList  -> Left "Erro de tipo: uma lista vazia não tem index"
                _           -> Left $ "Erro de tipo: espera uma lista para checar o index " ++ show tList

    -- Operadores Unários (-x, !b)
    UnaryOp Neg e -> do
        t <- typeCheckExpr env e
        if t == TInt || t == TFloat
            then Right t
            else Left $ "Erro de tipo: (neg op) precisa ser tipo Int ou Float, recebeu " ++ show t

    UnaryOp Not e -> do
        t <- typeCheckExpr env e
        if t == TBool
            then Right TBool
            else Left $ "Erro de tipo: (not op) precisa ser tipo Bool, recebeu " ++ show t

    -- Condicional (IfThenElse) com unificação dos ramos 
    -- (permite then [] else [1, 2], já que emptyList consegue se unir com qualquer lista)
    IfThenElse cond thenExpr elseExpr -> do
        tCond <- typeCheckExpr env cond
        if tCond /= TBool
            then Left $ "Erro de tipo: A condição do 'if' deve ser Bool, mas recebeu " ++ show tCond
            else do
                tThen <- typeCheckExpr env thenExpr
                tElse <- typeCheckExpr env elseExpr
                unifyTypes tThen tElse -- checa se e1 == e2 no if

    -- Let
    Let decls body -> do
        localEnv <- foldM typeCheckDecl env decls -- checa as declarações locais e atualiza o ambiente
        typeCheckExpr localEnv body -- cria novo env local para o corpo do let

    -- Chamada de Função
    -- Pode ser Builtin ou TFunc
    Call name args -> case Map.lookup name env of
--        Just (TBuiltin builtin) -> do -- por que usar JUST nesse caso e não nos outros?
--            argTypes <- mapM (typeCheckExpr env) args
--            typeCheckBuiltin builtin argTypes

        Just (TFunc paramTypes returnType) -> do
            argTypes <- mapM (typeCheckExpr env) args
            if length argTypes /= length paramTypes
                then Left $ "Erro de tipo: A função '" ++ name ++ "' esperava "
                         ++ show (length paramTypes) ++ " argumentos, mas recebeu "
                         ++ show (length argTypes)
                else do
                    _ <- zipWithM unifyTypes paramTypes argTypes -- tenta unificar os tipos de parâmetros da chamada e da anotação
                    -- o que é zipWithM
                    Right returnType

        Just _  -> Left $ "Erro de tipo: '" ++ name ++ "' não é uma função."
        Nothing -> Left $ "Erro de tipo: Função não declarada '" ++ name ++ "'."

    -- Operadores Binários
    BinaryOp op e1 e2 -> do
        t1 <- typeCheckExpr env e1
        t2 <- typeCheckExpr env e2
        typeCheckBinaryOp op t1 t2 -- checa se os operadores são válidos para t1 e t2

--------------------------------------------------------------------------------
-- 4. REGRAS DE TIPAGEM PARA PRIMITIVAS (head, tail, len)
--------------------------------------------------------------------------------

-- typeCheckBuiltin :: BuiltinOp -> [Type] -> Either String Type
-- implementar depois

--------------------------------------------------------------------------------
-- 5. REGRAS PARA OPERADORES BINÁRIOS
--------------------------------------------------------------------------------

-- Funções de checagem de operações binárias

checkArithmetic :: Type -> Type -> Either String Type
checkArithmetic TInt TInt = Right TInt
checkArithmetic TFloat TFloat = Right TFloat
checkArithmetic t1 t2 = Left $ "Erro de tipo: Operador aritmético, incompatível com " ++ show t1 ++ " e " ++ show t2

checkModulo :: Type -> Type -> Either String Type
checkModulo TInt TInt = Right TInt
checkModulo t1 t2 = Left $ "Erro de tipo: Operador '%' exige que ambos sejam Int, e tem " ++ show t1 ++ " e " ++ show t2

checkCons :: Type -> Type -> Either String Type
checkCons t1 (TList elemsT) = TList <$> unifyTypes elemsT t1
checkCons t1 TEmptyList = Right (TList t1)
checkCons _ t2 = Left $ "Erro de tipo: O lado direito de ':' deve ser uma lista " ++ show t2

checkConcat :: Type -> Type -> Either String Type
checkConcat (TList t1) (TList t2) = TList <$> unifyTypes t1 t2
checkConcat (TList t1) TEmptyList = Right t1
checkConcat TEmptyList (TList t2) = Right t2
checkConcat TEmptyList TEmptyList = Right TEmptyList
checkConcat TString TString = Right TString
checkConcat t1 t2 = Left $ "Erro de tipo: Operador '++', precisa de listas ou strings, e tem " ++ show t1 ++ " e " ++ show t2

checkLogical :: Type -> Type -> Either String Type
checkLogical TBool TBool = Right TBool
checkLogical t1 t2 = Left $ "Erro de tipo: precisa de dois Bools, tipos de e1 e e2:" ++ show t1 ++ " e " ++ show t2

checkEquality :: Type -> Type -> Either String Type
checkEquality t1 t2 = do
    unified <- unifyTypes t1 t2
    if isNonComparable unified
        then Left $ "Erro de tipo: tipos não comparáveis: "
            ++ show t1 ++ " e " ++ show t2
        else Right TBool

checkRelationals :: Type -> Type -> Either String Type
checkRelationals TInt TInt = Right TBool
checkRelationals TFloat TFloat = Right TBool
checkRelationals t1 t2 = Left $ "Erro de tipo: Operador de relação exige que t1 == t2 (TInt ou TFloat) " ++ show t1 ++ " e " ++ show t2

typeCheckBinaryOp :: BinOp -> Type -> Type -> Either String Type
typeCheckBinaryOp op t1 t2 = case op of

    -- Operadores aritméticos Int ou Float
    Add -> checkArithmetic t1 t2
    Sub -> checkArithmetic t1 t2
    Mul -> checkArithmetic t1 t2
    Div -> checkArithmetic t1 t2
    Pow -> checkArithmetic t1 t2

    -- Módulo (%) Int
    Mod -> checkModulo t1 t2

    -- Operador Cons (:): aceita TList elemT e também TEmptyList (ex: 1 : [])
    Cons -> checkCons t1 t2

    -- Concatenação de Listas (++): aceita [1,2] ++ [], [] ++ [1,2] e [] ++ []
    Concat -> checkConcat t1 t2

    -- Operadores Lógicos (&&, ||)
    And -> checkLogical t1 t2
    Or -> checkLogical t1 t2

    -- Igualdade e Desigualdade (==, !=): unifica tipos (ex: xs == []) e barra funções
    Eq -> checkEquality t1 t2
    Neq -> checkEquality t1 t2

    -- Ordenação (<, >, <=, >=): exclusivamente numérica (Int ou Float)
    Lt -> checkRelationals t1 t2
    Gt -> checkRelationals t1 t2
    Leq -> checkRelationals t1 t2
    Geq -> checkRelationals t1 t2

    _ -> Left $ "Erro de tipo: Operador não tratado " ++ show op

--------------------------------------------------------------------------------
-- 6. CHECAGEM DE DECLARAÇÕES
--------------------------------------------------------------------------------

-- É nessa função que atualizamos o env com assingments de variáveis, anotação de funções e etc
typeCheckDecl :: TypeEnv -> Declaration -> Either String TypeEnv
typeCheckDecl env decl = case decl of

    -- Anotação de função:
    -- Se tem 1 tipo (ex: xs :: [Int]), registra variável;
    -- Se tem >= 2 tipos (ex: soma :: [Int] -> Int), registra assinatura de função.
    FuncAnnot name typeExprs -> case map typeExprToType typeExprs of
        []       -> Left $ "Erro de tipo: Anotação vazia para " ++ name
        [varT]   -> Right (Map.insert name varT env)
        allTypes ->
            let paramTypes = init allTypes
                returnType = last allTypes
            in Right (Map.insert name (TFunc paramTypes returnType) env)

    -- Definição de função (suporta recursão pois 'name' já está em 'env')
    FuncDef name params body -> case Map.lookup name env of
        Just (TFunc paramTypes returnType) ->
            if length params /= length paramTypes
                then Left $ "Erro de tipo: Definição de '" ++ name ++ "' tem " ++ show (length params) ++ " parâmetros, mas a assinatura declara " ++ show (length paramTypes)
                else do
                    let paramBindings = Map.fromList (zip params paramTypes)
                    let localEnv = Map.union paramBindings env
                    bodyType <- typeCheckExpr localEnv body
                    _ <- unifyTypes returnType bodyType
                    Right env
        Just _  -> Left $ "Erro de tipo: '" ++ name ++ "' foi anotado como variável, mas definido como função."
        Nothing -> Left $ "Erro de tipo: A função '" ++ name ++ "' precisa de anotação de tipo antes da definição."

    -- Atribuição simples (=) e composta (+=, -=, *=, /=)
    Assignment name Assign expr -> do
        tExpr <- typeCheckExpr env expr
        case Map.lookup name env of
            Nothing -> Right (Map.insert name tExpr env)
            Just prevT -> do
                unifiedT <- unifyTypes prevT tExpr
                Right (Map.insert name unifiedT env)

    Assignment name op expr -> case Map.lookup name env of
        Nothing -> Left $ "Erro de tipo: Variável '" ++ name ++ "' não declarada para operador composto."
        Just varT -> do
            tExpr <- typeCheckExpr env expr
            if (varT == TInt || varT == TFloat) && varT == tExpr
                then Right env
                else Left $ "Erro de tipo: Operador '" ++ show op ++ "' incompatível com " ++ show varT ++ " e " ++ show tExpr

typeCheckStatement :: TypeEnv -> Statement -> Either String (Type, TypeEnv)
typeCheckStatement env (ExprStatement expr) = do
    t <- typeCheckExpr env expr
    Right (t, env)
typeCheckStatement env (DeclarationStatement decl) = do
    newEnv <- typeCheckDecl env decl
    Right (TUnit, newEnv)

typeCheckProgram :: TypeEnv -> Program -> Either String TypeEnv
typeCheckProgram env [] = Right env
typeCheckProgram env (stmt:stmts) = do
    (_, newEnv) <- typeCheckStatement env stmt
    typeCheckProgram newEnv stmts

typeCheckProgramDefault :: Program -> Either String TypeEnv
typeCheckProgramDefault = typeCheckProgram initialTypeEnv