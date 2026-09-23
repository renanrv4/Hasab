{- 

Identifier
Integer
Float
String
Boolean

Operadores = ["||", "&&", "=", "==", "!=", ">", "<", ">=", "<=", "+", "-", "*", "/", "%"]

Delimitadores = ["(", ")", "[", "]"]

-}

data Token = 
    Identifier String 
    | IntegerLiteral Integer 
    | FloatLiteral Double 
    | StringLiteral String 
    | BooleanLiteral Bool 
    | Plus
    | Minus
    | Multiply
    | Divide
    | Modulo
    | Equals
    | EqualsEquals
    | Different
    | Greater
    | Less
    | GreaterEquals
    | LessEquals
    | And
    | Or
    | LeftParent
    | RightParent
    | LeftBracket
    | RightBracket
    | Comma
    | ColonColon
    deriving(Show, Eq)
