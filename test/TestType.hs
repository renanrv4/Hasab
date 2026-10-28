module Main where

import AST
import Parser
import TypeChecker
import Test.HUnit
import Text.Megaparsec
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- 1. AMBIENTE DE SIMULAÇÃO E AUXILIARES
--------------------------------------------------------------------------------

-- Ambiente com variáveis e funções pré-declaradas para os testes
envTeste :: TypeEnv
envTeste = Map.fromList
    [ ("x", TInt)
    , ("y", TFloat)
    , ("flag", TBool)
    , ("msg", TString)
    , ("soma", TFunc [TInt, TInt] TInt)            -- soma :: Int -> Int -> Int
    , ("ehPar", TFunc [TInt] TBool)               -- ehPar :: Int -> Bool
    ]

-- Executa o parser de expressões
parseExpr :: String -> Either String Expr
parseExpr input = case parse (pExpression <* eof) "" input of
    Left err  -> Left (errorBundlePretty err)
    Right ast -> Right ast

-- Checa o tipo de uma expressão usando o ambiente de teste
checkExpr :: String -> Either String Type
checkExpr input = case parseExpr input of
    Left err  -> Left ("Erro de sintaxe: " ++ err)
    Right ast -> typeCheckExpr envTeste ast

--------------------------------------------------------------------------------
-- 2. TESTES DE LITERAIS
--------------------------------------------------------------------------------

testesLiterais :: Test
testesLiterais = TestList
    [ TestLabel "Literal Inteiro" $ TestCase $ do
        assertEqual "AST Int" (Right (LitInt 42)) (parseExpr "42")
        assertEqual "Tipo Int" (Right TInt) (checkExpr "42")

    , TestLabel "Literal Float" $ TestCase $ do
        assertEqual "AST Float" (Right (LitFloat 3.14)) (parseExpr "3.14")
        assertEqual "Tipo Float" (Right TFloat) (checkExpr "3.14")

    , TestLabel "Literal Bool (true/false)" $ TestCase $ do
        assertEqual "AST Bool true" (Right (LitBool True)) (parseExpr "true")
        assertEqual "Tipo Bool true" (Right TBool) (checkExpr "true")
        assertEqual "AST Bool false" (Right (LitBool False)) (parseExpr "false")
        assertEqual "Tipo Bool false" (Right TBool) (checkExpr "false")

    , TestLabel "Literal String" $ TestCase $ do
        assertEqual "AST String" (Right (LitString "Haskat")) (parseExpr "\"Haskat\"")
        assertEqual "Tipo String" (Right TString) (checkExpr "\"Haskat\"")
    ]

--------------------------------------------------------------------------------
-- 3. TESTES DE VARIÁVEIS
--------------------------------------------------------------------------------

testesVariaveis :: Test
testesVariaveis = TestList
    [ TestLabel "Variável Declarada" $ TestCase $ do
        assertEqual "AST Var x" (Right (Var "x")) (parseExpr "x")
        assertEqual "Tipo Var x" (Right TInt) (checkExpr "x")
        assertEqual "Tipo Var flag" (Right TBool) (checkExpr "flag")

    , TestLabel "Variável Não Declarada (Erro)" $ TestCase $ do
        assertBool "Deve falhar para variável inexistente" $
            case checkExpr "variavelInexistente" of
                Left _  -> True
                Right _ -> False
    ]

--------------------------------------------------------------------------------
-- 4. TESTES DE LISTAS
--------------------------------------------------------------------------------

testesListas :: Test
testesListas = TestList
    [ TestLabel "Lista Homogênea de Ints" $ TestCase $ do
        let expr = "[1, 2, 3]"
        assertEqual "AST Lista Int"
            (Right (ListExpr [LitInt 1, LitInt 2, LitInt 3]))
            (parseExpr expr)
        assertEqual "Tipo Lista Int"
            (Right (TList TInt))
            (checkExpr expr)

    , TestLabel "Lista Heterogênea (Erro Semântico)" $ TestCase $ do
        assertBool "Deve falhar para lista de tipos misturados [1, true]" $
            case checkExpr "[1, true]" of
                Left _  -> True
                Right _ -> False
    ]

--------------------------------------------------------------------------------
-- 5. TESTES DE ESTRUTURAS CONDICIONAIS (IF-THEN-ELSE)
--------------------------------------------------------------------------------

testesCondicionais :: Test
testesCondicionais = TestList
    [ TestLabel "If-Then-Else Válido" $ TestCase $ do
        let expr = "if x > 0 then 1 else 0"
        assertEqual "AST If-Then-Else"
            (Right (IfThenElse 
                (BinaryOp ">" (Var "x") (LitInt 0)) 
                (LitInt 1) 
                (LitInt 0)))
            (parseExpr expr)
        assertEqual "Tipo If-Then-Else"
            (Right TInt)
            (checkExpr expr)

    , TestLabel "If com Condição Não-Bool (Erro)" $ TestCase $ do
        assertBool "Deve falhar se a condição do 'if' não for Bool" $
            case checkExpr "if 10 then 1 else 0" of
                Left _  -> True
                Right _ -> False

    , TestLabel "If com Ramos Incompatíveis (Erro)" $ TestCase $ do
        assertBool "Deve falhar se 'then' e 'else' tiverem tipos diferentes" $
            case checkExpr "if true then 1 else false" of
                Left _  -> True
                Right _ -> False
    ]

--------------------------------------------------------------------------------
-- 6. TESTES DE CHAMADAS DE FUNÇÃO
--------------------------------------------------------------------------------

testesChamadaFuncao :: Test
testesChamadaFuncao = TestList
    [ TestLabel "Chamada Válida soma(x, 10)" $ TestCase $ do
        let expr = "soma(x, 10)"
        assertEqual "AST Call"
            (Right (Call "soma" [Var "x", LitInt 10]))
            (parseExpr expr)
        assertEqual "Tipo de Retorno da Função"
            (Right TInt)
            (checkExpr expr)

    , TestLabel "Chamada com Tipos de Argumento Errados (Erro)" $ TestCase $ do
        assertBool "Deve falhar ao passar Bool onde esperava Int" $
            case checkExpr "soma(true, 10)" of
                Left _  -> True
                Right _ -> False
    ]

--------------------------------------------------------------------------------
-- 7. TESTES DE BLOCO LET
--------------------------------------------------------------------------------

testesLet :: Test
testesLet = TestList
    [ TestLabel "Parser do Bloco Let" $ TestCase $ do
        let expr = "let a = 5; in a + 1"
        assertEqual "AST Let"
            (Right (Let [Assignment "a" "=" (LitInt 5)] (BinaryOp "+" (Var "a") (LitInt 1))))
            (parseExpr expr)
    ]

--------------------------------------------------------------------------------
-- 8. SUÍTE COMPLETA DE OPERADORES BINÁRIOS (16 OPERADORES)
--------------------------------------------------------------------------------

testesOperadores :: Test
testesOperadores = TestList
    [ TestLabel "Exponenciação (^)" $ TestCase $
        assertEqual "2 ^ 3" (Right TInt) (checkExpr "2 ^ 3")
    , TestLabel "Multiplicação (*)" $ TestCase $
        assertEqual "4 * 5" (Right TInt) (checkExpr "4 * 5")
    , TestLabel "Divisão (/)" $ TestCase $
        assertEqual "10.0 / 2.0" (Right TFloat) (checkExpr "10.0 / 2.0")
    , TestLabel "Módulo (%)" $ TestCase $
        assertEqual "10 % 3" (Right TInt) (checkExpr "10 % 3")
    , TestLabel "Adição (+)" $ TestCase $
        assertEqual "7 + 3" (Right TInt) (checkExpr "7 + 3")
    , TestLabel "Subtração (-)" $ TestCase $
        assertEqual "15 - 4" (Right TInt) (checkExpr "15 - 4")
    , TestLabel "Cons (:)" $ TestCase $
        assertEqual "1 : [2, 3]" (Right (TList TInt)) (checkExpr "1 : [2, 3]")
    , TestLabel "Concatenação (++)" $ TestCase $
        assertEqual "[1, 2] ++ [3, 4]" (Right (TList TInt)) (checkExpr "[1, 2] ++ [3, 4]")
    , TestLabel "Maior ou Igual (>=)" $ TestCase $
        assertEqual "5 >= 3" (Right TBool) (checkExpr "5 >= 3")
    , TestLabel "Menor ou Igual (<=)" $ TestCase $
        assertEqual "2 <= 4" (Right TBool) (checkExpr "2 <= 4")
    , TestLabel "Maior que (>)" $ TestCase $
        assertEqual "10 > 2" (Right TBool) (checkExpr "10 > 2")
    , TestLabel "Menor que (<)" $ TestCase $
        assertEqual "1 < 8" (Right TBool) (checkExpr "1 < 8")
    , TestLabel "Igualdade (==)" $ TestCase $
        assertEqual "5 == 5" (Right TBool) (checkExpr "5 == 5")
    , TestLabel "Diferença (!=)" $ TestCase $
        assertEqual "5 != 3" (Right TBool) (checkExpr "5 != 3")
    , TestLabel "E Lógico (&&)" $ TestCase $
        assertEqual "true && false" (Right TBool) (checkExpr "true && false")
    , TestLabel "OU Lógico (||)" $ TestCase $
        assertEqual "true || false" (Right TBool) (checkExpr "true || false")
    ]

--------------------------------------------------------------------------------
-- RUNNER PRINCIPAL
--------------------------------------------------------------------------------

todosTestes :: Test
todosTestes = TestList
    [ testesLiterais
    , testesVariaveis
    , testesListas
    , testesCondicionais
    , testesChamadaFuncao
    , testesLet
    , testesOperadores
    ]

main :: IO ()
main = do
    putStrLn "=== Executando Suíte de Testes de Expressões do Hasab ==="
    _ <- runTestTT todosTestes
    return ()