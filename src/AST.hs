module AST
    ( BinOp(..)
    , UnaryOp(..)
    , AssignOp(..)
    , TypeExpr(..)
    , Expr(..)
    , Declaration(..)
    , Statement(..)
    , Program
    ) where

--------------------------------------------------------------------------------
-- 1. OPERADORES E ANOTAÇÕES DE TIPO (TIPAGEM FORTE NA AST)
--------------------------------------------------------------------------------

-- | Operadores binários suportados pela linguagem (16 operadores)
data BinOp
    = Add | Sub | Mul | Div | Pow | Mod   -- Aritméticos: +, -, *, /, ^, %
    | Cons | Concat                       -- Listas: :, ++
    | Eq | Neq | Lt | Gt | Leq | Geq      -- Relacionais: ==, !=, <, >, <=, >=
    | And | Or                            -- Lógicos: &&, ||
    deriving (Show, Eq)

-- | Operadores unários prefixos
data UnaryOp
    = Neg   -- Negação aritmética: -x
    | Not   -- Negação lógica: !x
    deriving (Show, Eq)

-- | Operadores de atribuição simples e composta
data AssignOp
    = Assign      -- =
    | AddAssign   -- +=
    | SubAssign   -- -=
    | MulAssign   -- *=
    | DivAssign   -- /=
    deriving (Show, Eq)

-- | Representação sintática das anotações de tipo (ex: Int, [Float])
data TypeExpr
    = TyInt
    | TyFloat
    | TyBool
    | TyString
    | TyList TypeExpr
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. EXPRESSÕES, DECLARAÇÕES E PROGRAMA
--------------------------------------------------------------------------------

data Expr
    = LitInt Integer
    | LitFloat Double
    | LitBool Bool
    | LitString String
    | EmptyList                     -- Lista vazia explícita: []
    | Var String
    | ListExpr [Expr]               -- Lista literal não-vazia: [e1, e2, ...]
    | Index Expr Expr               -- Acesso por índice: xs[i]
    | Call String [Expr]
    | UnaryOp UnaryOp Expr          -- Operação unária: -x, !b
    | BinaryOp BinOp Expr Expr      -- Operação binária tipada
    | Let [Declaration] Expr
    | IfThenElse Expr Expr Expr
    deriving (Show, Eq)

data Declaration
    = Assignment String AssignOp Expr
    | FuncAnnot String [TypeExpr]
    | FuncDef String [String] Expr
    deriving (Show, Eq)

data Statement
    = ExprStatement Expr
    | DeclarationStatement Declaration
    deriving (Show, Eq)

type Program = [Statement]