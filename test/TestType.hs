module Main where

import AST
import Parser
import TypeChecker
import Text.Megaparsec
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "=== Testando o Type Checker ==="
    
    -- Criamos um ambiente vazio (sem variáveis salvas por enquanto)
    let envVazio = Map.empty 

    -- Exemplo 1: Soma válida (Int + Int)
    let teste1 = "2 + 3"
    case parse (pExpression <* eof) "" teste1 of
        Right ast -> print $ typeCheckExpr envVazio ast  -- Deve retornar: Right TInt
        Left _ -> putStrLn "Erro no Parser"

    -- Exemplo 2: Erro Semântico (Bool + Int)
    let teste2 = "true + 3"
    case parse (pExpression <* eof) "" teste2 of
        Right ast -> print $ typeCheckExpr envVazio ast  -- Deve retornar: Left "Operador aritmético '+' incompatível..."
        Left _ -> putStrLn "Erro no Parser"