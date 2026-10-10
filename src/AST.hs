module AST ( Expr(..), Declaration(..), Statement(..), Program) where

--------------------------------------------------------------------------------
-- COMPONENTES DA AST
--------------------------------------------------------------------------------

data Expr
    = LitInt Integer
    | LitFloat Double
    | LitBool Bool
    | LitString String
    | Var String
    | ListExpr [Expr]
    | Call String [Expr]
    | BinaryOp String Expr Expr
    | Let [Declaration] Expr
    deriving (Show, Eq)

data Declaration
    = Assignment String String Expr
    | FuncAnnot String [String]
    | FuncDef String [String] Expr
    deriving (Show, Eq)

data Statement
    = ExprStatement Expr
    | DeclarationStatement Declaration
    deriving (Show, Eq)

type Program = [Statement]