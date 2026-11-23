module TypeChecker where

import AST
import Control.Monad (foldM, zipWithM)
import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- 1. DEFINIÇÃO DOS TIPOS ESTÁTICOS E PRIMITIVAS
--------------------------------------------------------------------------------

data BuiltinOp
    = BHead
    | BTail
    | BLen
    deriving (Show, Eq)

data Type
    = TInt
    | TFloat
    | TBool
    | TString
    | TEmptyList               -- Tipo polimórfico de lista vazia: []
    | TList Type               -- Lista tipada homogênea: [T]
    | TFunc [Type] Type        -- Assinatura de função: [ParamTypes] -> ReturnType
    | TBuiltin BuiltinOp       -- Primitivas polimórficas (head, tail, len)
    | TUnit                    -- Tipo de instruções que apenas alteram ambiente
    deriving (Show, Eq)

type TypeEnv = Map String Type

-- | Ambiente estático inicial com as primitivas de lista
initialTypeEnv :: TypeEnv
initialTypeEnv = Map.fromList
    [ ("head", TBuiltin BHead)
    , ("tail", TBuiltin BTail)
    , ("len",  TBuiltin BLen)
    ]

-- | Converte a anotação sintática da AST (TypeExpr) para o tipo semântico (Type)
typeExprToType :: TypeExpr -> Type
typeExprToType TyInt       = TInt
typeExprToType TyFloat     = TFloat
typeExprToType TyBool      = TBool
typeExprToType TyString    = TString
typeExprToType (TyList te) = TList (typeExprToType te)

--------------------------------------------------------------------------------
-- 2. UNIFICAÇÃO DE TIPOS (SUPORTE POLIMÓRFICO À LISTA VAZIA [])
--------------------------------------------------------------------------------

-- | Unifica dois tipos compatíveis, resolvendo TEmptyList contra qualquer TList t
unifyTypes :: Type -> Type -> Either String Type
unifyTypes TEmptyList (TList t)  = Right (TList t)
unifyTypes (TList t) TEmptyList  = Right (TList t)
unifyTypes TEmptyList TEmptyList = Right TEmptyList
unifyTypes (TList a) (TList b)   = TList <$> unifyTypes a b
unifyTypes (TFunc p1 r1) (TFunc p2 r2)
    | length p1 == length p2 = TFunc <$> zipWithM unifyTypes p1 p2 <*> unifyTypes r1 r2
    | otherwise = Left "Aridades de função incompatíveis."
unifyTypes t1 t2
    | t1 == t2  = Right t1
    | otherwise = Left $ "Incompatibilidade de tipos: esperado " ++ show t1 ++ ", mas encontrou " ++ show t2

-- | Verifica se um tipo contém funções ou Unit (não comparáveis com == / !=)
isNonComparable :: Type -> Bool
isNonComparable (TFunc _ _)  = True
isNonComparable (TBuiltin _) = True
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
    Var name -> case Map.lookup name env of
        Just t  -> Right t
        Nothing -> Left $ "Erro de tipo: Variável não declarada '" ++ name ++ "'."

    -- Listas não-vazias (unifica todos os elementos, inclusive listas aninhadas com [])
    ListExpr [] -> Right TEmptyList
    ListExpr (e:es) -> do
        firstT <- typeCheckExpr env e
        elemT  <- foldM (\acc x -> typeCheckExpr env x >>= unifyTypes acc) firstT es
        Right (TList elemT)

    -- Indexação de listas: xs[i]
    Index listExpr idxExpr -> do
        tList <- typeCheckExpr env listExpr
        tIdx  <- typeCheckExpr env idxExpr
        if tIdx /= TInt
            then Left $ "Erro de tipo: O índice da lista deve ser Int, mas recebeu " ++ show tIdx
            else case tList of
                TList elemT -> Right elemT
                TEmptyList  -> Left "Erro de tipo: Não é possível indexar uma lista vazia."
                _           -> Left $ "Erro de tipo: Esperava uma lista para indexação, recebeu " ++ show tList

    -- Operadores Unários (-x, !b)
    UnaryOp Neg e -> do
        t <- typeCheckExpr env e
        if t == TInt || t == TFloat
            then Right t
            else Left $ "Erro de tipo: Operador unário '-' espera Int ou Float, recebeu " ++ show t

    UnaryOp Not e -> do
        t <- typeCheckExpr env e
        if t == TBool
            then Right TBool
            else Left $ "Erro de tipo: Operador unário '!' espera Bool, recebeu " ++ show t

    -- Condicional (IfThenElse) com unificação dos ramos (permite then [] else [1, 2])
    IfThenElse cond thenExpr elseExpr -> do
        tCond <- typeCheckExpr env cond
        if tCond /= TBool
            then Left $ "Erro de tipo: A condição do 'if' deve ser Bool, mas recebeu " ++ show tCond
            else do
                tThen <- typeCheckExpr env thenExpr
                tElse <- typeCheckExpr env elseExpr
                unifyTypes tThen tElse

    -- Bloco Let (Escopo local isolado)
    Let decls body -> do
        localEnv <- foldM typeCheckDecl env decls
        typeCheckExpr localEnv body

    -- Chamada de Função (Usuário ou Primitiva)
    Call name args -> case Map.lookup name env of
        Just (TBuiltin builtin) -> do
            argTypes <- mapM (typeCheckExpr env) args
            typeCheckBuiltin builtin argTypes

        Just (TFunc paramTypes returnType) -> do
            argTypes <- mapM (typeCheckExpr env) args
            if length argTypes /= length paramTypes
                then Left $ "Erro de tipo: A função '" ++ name ++ "' esperava "
                         ++ show (length paramTypes) ++ " argumentos, mas recebeu "
                         ++ show (length argTypes)
                else do
                    _ <- zipWithM unifyTypes paramTypes argTypes
                    Right returnType

        Just _  -> Left $ "Erro de tipo: '" ++ name ++ "' não é uma função."
        Nothing -> Left $ "Erro de tipo: Função não declarada '" ++ name ++ "'."

    -- Operadores Binários
    BinaryOp op e1 e2 -> do
        t1 <- typeCheckExpr env e1
        t2 <- typeCheckExpr env e2
        typeCheckBinaryOp op t1 t2

--------------------------------------------------------------------------------
-- 4. REGRAS DE TIPAGEM PARA PRIMITIVAS (head, tail, len)
--------------------------------------------------------------------------------

typeCheckBuiltin :: BuiltinOp -> [Type] -> Either String Type
typeCheckBuiltin BHead [TList elemT] = Right elemT
typeCheckBuiltin BHead [TEmptyList]  = Left "Erro de tipo: 'head' não pode ser aplicado a uma lista vazia."
typeCheckBuiltin BHead args          = Left $ "Erro de tipo: 'head' espera [a], recebeu " ++ show args

typeCheckBuiltin BTail [TList elemT] = Right (TList elemT)
typeCheckBuiltin BTail [TEmptyList]  = Left "Erro de tipo: 'tail' não pode ser aplicado a uma lista vazia."
typeCheckBuiltin BTail args          = Left $ "Erro de tipo: 'tail' espera [a], recebeu " ++ show args

typeCheckBuiltin BLen [TList _]    = Right TInt
typeCheckBuiltin BLen [TEmptyList] = Right TInt
typeCheckBuiltin BLen args         = Left $ "Erro de tipo: 'len' espera uma lista, recebeu " ++ show args

--------------------------------------------------------------------------------
-- 5. REGRAS PARA OPERADORES BINÁRIOS (16 OPERADORES CORRIGIDOS)
--------------------------------------------------------------------------------

typeCheckBinaryOp :: BinOp -> Type -> Type -> Either String Type
typeCheckBinaryOp op t1 t2 = case op of

    -- Aritméticos (+, -, *, /, ^): Int ou Float homogêneos
    _ | op `elem` [Add, Sub, Mul, Div, Pow] ->
        if (t1 == TInt || t1 == TFloat) && t1 == t2
            then Right t1
            else Left $ "Erro de tipo: Operador aritmético '" ++ show op ++ "' incompatível com "
                     ++ show t1 ++ " e " ++ show t2

    -- Módulo (%): exclusivamente Int
    Mod ->
        if t1 == TInt && t2 == TInt
            then Right TInt
            else Left $ "Erro de tipo: Operador '%' exige dois Ints, recebeu "
                     ++ show t1 ++ " e " ++ show t2

    -- Operador Cons (:): aceita tanto TList elemT quanto TEmptyList à direita (ex: 1 : [])
    Cons -> case t2 of
        TList elemT -> TList <$> unifyTypes elemT t1
        TEmptyList  -> Right (TList t1)
        _           -> Left $ "Erro de tipo: O lado direito de ':' deve ser uma lista, recebeu " ++ show t2

    -- Concatenação de Listas (++): aceita [1,2] ++ [], [] ++ [1,2] e [] ++ []
    Concat -> case (t1, t2) of
        (TList _, TList _)   -> unifyTypes t1 t2
        (TList _, TEmptyList) -> Right t1
        (TEmptyList, TList _) -> Right t2
        (TEmptyList, TEmptyList) -> Right TEmptyList
        _ -> Left $ "Erro de tipo: Operador '++' exige duas listas, recebeu " ++ show t1 ++ " e " ++ show t2

    -- Operadores Lógicos (&&, ||)
    _ | op `elem` [And, Or] ->
        if t1 == TBool && t2 == TBool
            then Right TBool
            else Left $ "Erro de tipo: Operador lógico '" ++ show op ++ "' exige Bools, recebeu "
                     ++ show t1 ++ " e " ++ show t2

    -- Igualdade e Desigualdade (==, !=): unifica tipos (ex: xs == []) e barra funções
    _ | op `elem` [Eq, Neq] ->
        if isNonComparable t1 || isNonComparable t2
            then Left "Erro de tipo: Funções e Unit não podem ser comparados com '==' ou '!='."
            else do
                _ <- unifyTypes t1 t2
                Right TBool

    -- Ordenação (<, >, <=, >=): exclusivamente numérica (Int ou Float)
    _ | op `elem` [Lt, Gt, Leq, Geq] ->
        if (t1 == TInt || t1 == TFloat) && t1 == t2
            then Right TBool
            else Left $ "Erro de tipo: Operador de ordenação '" ++ show op ++ "' exige tipos numéricos iguais, recebeu "
                     ++ show t1 ++ " e " ++ show t2

    _ -> Left $ "Erro de tipo: Operador não tratado " ++ show op

--------------------------------------------------------------------------------
-- 6. CHECAGEM DE DECLARAÇÕES, FUNÇÕES RECURSIVAS E PROGRAMAS
--------------------------------------------------------------------------------

typeCheckDecl :: TypeEnv -> Declaration -> Either String TypeEnv
typeCheckDecl env decl = case decl of

    -- Anotação de tipo:
    -- Se tem 1 tipo (ex: xs :: [Int]), registra variável;
    -- Se tem >= 2 tipos (ex: soma :: [Int] -> Int), registra assinatura de função.
    FuncAnnot name typeExprs -> case map typeExprToType typeExprs of
        []       -> Left $ "Erro de tipo: Anotação vazia para '" ++ name ++ "'."
        [varT]   -> Right (Map.insert name varT env)
        allTypes ->
            let paramTypes = init allTypes
                returnType = last allTypes
            in Right (Map.insert name (TFunc paramTypes returnType) env)

    -- Definição de função (suporta recursão pois 'name' já está em 'env')
    FuncDef name params body -> case Map.lookup name env of
        Just (TFunc paramTypes returnType) ->
            if length params /= length paramTypes
                then Left $ "Erro de tipo: Definição de '" ++ name ++ "' tem " ++ show (length params)
                         ++ " parâmetros, mas a assinatura declara " ++ show (length paramTypes)
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
                else Left $ "Erro de tipo: Operador '" ++ show op ++ "' incompatível com "
                         ++ show varT ++ " e " ++ show tExpr

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

-- | Atalho para validar um programa completo partindo do ambiente padrão
typeCheckProgramDefault :: Program -> Either String TypeEnv
typeCheckProgramDefault = typeCheckProgram initialTypeEnv