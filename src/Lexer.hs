module Lexer where

import Data.Char

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
    | NotEquals
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
    | Arrow
    
    deriving(Show, Eq)

{-

ERROS DO LEXER

-}

data LexerErrors =
    UnexpectedChar Char
    | InvalidNumber
    deriving(Show, Eq)

{-

PEGANDO TOKENS DE IDENTIFICADORES E BOOLEANOS

-}

getName :: String -> (String, String)
getName input = getNameAux [] input
    where
        getNameAux name [] = (name, [])
        getNameAux snow (x:xs)
            | isAlphaNum x || x == '_' = getNameAux (snow ++ [x]) xs
            | otherwise = (snow, x:xs)

getToken :: String -> Token
getToken "true" = BooleanLiteral True
getToken "false" = BooleanLiteral False
getToken name = Identifier name

getTokenFromName (y, ys) = addToken (getToken y) (lexer ys)

{-

PEGANDO TOKENS DE INTEIROS E FLOATS

-}

checkdot :: String -> Bool
checkdot [] = False
checkdot (x:xs)
    | x == '.' = True
    | otherwise = checkdot xs

getNumberToken :: String -> Token
getNumberToken num
    | checkdot num = FloatLiteral (read num)
    | otherwise = IntegerLiteral (read num)

getNumber :: String -> Either LexerErrors (String, String)
getNumber input = getNumberAux [] False input
    where
        getNumberAux num hasdot [] = Right (num, [])
        getNumberAux num hasdot (x:xs)
          | isDigit x = getNumberAux (num ++ [x]) hasdot xs
          | x == '.' && not hasdot = getNumberAux (num ++ [x]) True xs
          | x == '.' && hasdot = Left InvalidNumber
          | isAlpha x = Left InvalidNumber
          | otherwise = Right (num, x:xs)

getTokenFromNumber :: Either LexerErrors (String, String) -> Either LexerErrors [Token]
getTokenFromNumber (Left err) = Left err
getTokenFromNumber (Right (num, xs)) = addToken (getNumberToken num) (lexer xs)

addToken token (Left err) = Left err
addToken token (Right tokens) = Right (token : tokens)

{-

Ainda falta implementar a lógica pra esses:
==, !=, >=, <=, &&, ||, ::, ->

-}

lexer :: String -> Either LexerErrors [Token]
lexer [] = Right []
lexer (x:xs)
    | isSpace x = lexer xs
    | isAlpha x = getTokenFromName(getName(x:xs))
    | isDigit x = getTokenFromNumber(getNumber(x:xs))
    | x == '+' = addToken Plus (lexer xs)
    | x == '-' = addToken Minus (lexer xs)
    | x == '*' = addToken Multiply (lexer xs)
    | x == '/' = addToken Divide (lexer xs)
    | x == '%' = addToken Modulo (lexer xs)
    | otherwise = Left (UnexpectedChar x)