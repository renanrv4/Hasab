module Main where

import AST
import Parser
import Interpreter
import Test.HUnit
import Text.Megaparsec
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- FUNÇÕES AUXILIARES DE TESTE
--------------------------------------------------------------------------------

-- | Executa o Parser de Expressão e avalia no Interpretador
evalExprStr :: String -> Either String Value
evalExprStr input = case parse (pExpression <* eof) "" input of
    Left err  -> Left ("Erro de sintaxe: " ++ errorBundlePretty err)
    Right ast -> evalExpr Map.empty ast

-- | Executa o Parser de Programa (Multilinha) e retorna o resultado e o estado
evalProgramStr :: String -> Either String (Value, RuntimeEnv)
evalProgramStr input = case parse pProgram "" input of
    Left err  -> Left ("Erro de sintaxe: " ++ errorBundlePretty err)
    Right ast -> evalProgram Map.empty ast

--------------------------------------------------------------------------------
-- 1. TESTES DE LITERAIS E VARIÁVEIS
--------------------------------------------------------------------------------

testesLiterais :: Test
testesLiterais = TestList
    [ TestLabel "Literal Inteiro" $ TestCase $
        assertEqual "42" (Right (VInt 42)) (evalExprStr "42")

    , TestLabel "Literal Float" $ TestCase $
        assertEqual "3.14" (Right (VFloat 3.14)) (evalExprStr "3.14")

    , TestLabel "Literal Bool" $ TestCase $ do
        assertEqual "true" (Right (VBool True)) (evalExprStr "true")
        assertEqual "false" (Right (VBool False)) (evalExprStr "false")

    , TestLabel "Literal String" $ TestCase $
        assertEqual "\"Hasab\"" (Right (VString "Hasab")) (evalExprStr "\"Hasab\"")
    ]

--------------------------------------------------------------------------------
-- 2. TESTES DE OPERADORES ARITMÉTICOS, EXPONENCIAÇÃO E MÓDULO
--------------------------------------------------------------------------------

testesOperadoresAritmeticos :: Test
testesOperadoresAritmeticos = TestList
    [ TestLabel "Adição (+)" $ TestCase $
        assertEqual "7 + 3" (Right (VInt 10)) (evalExprStr "7 + 3")

    , TestLabel "Subtração (-)" $ TestCase $
        assertEqual "15 - 5" (Right (VInt 10)) (evalExprStr "15 - 5")

    , TestLabel "Multiplicação (*)" $ TestCase $
        assertEqual "4 * 3" (Right (VInt 12)) (evalExprStr "4 * 3")

    , TestLabel "Divisão Inteira (/)" $ TestCase $
        assertEqual "10 / 2" (Right (VInt 5)) (evalExprStr "10 / 2")

    , TestLabel "Exponenciação (^)" $ TestCase $
        assertEqual "2 ^ 3" (Right (VInt 8)) (evalExprStr "2 ^ 3")

    , TestLabel "Módulo (%)" $ TestCase $
        assertEqual "10 % 3" (Right (VInt 1)) (evalExprStr "10 % 3")

    , TestLabel "Precedência de Operadores" $ TestCase $
        assertEqual "2 + 3 * 4" (Right (VInt 14)) (evalExprStr "2 + 3 * 4")
    ]

--------------------------------------------------------------------------------
-- 3. TESTES DE OPERADORES EM LISTAS (: e ++)
--------------------------------------------------------------------------------

testesListas :: Test
testesListas = TestList
    [ TestLabel "Construção de Lista ([])" $ TestCase $
        assertEqual "[1, 2, 3]" (Right (VList [VInt 1, VInt 2, VInt 3])) (evalExprStr "[1, 2, 3]")

    , TestLabel "Operador Cons (:)" $ TestCase $
        assertEqual "1 : [2, 3]" (Right (VList [VInt 1, VInt 2, VInt 3])) (evalExprStr "1 : [2, 3]")

    , TestLabel "Concatenação de Listas (++)" $ TestCase $
        assertEqual "[1, 2] ++ [3, 4]" (Right (VList [VInt 1, VInt 2, VInt 3, VInt 4])) (evalExprStr "[1, 2] ++ [3, 4]")
    ]

--------------------------------------------------------------------------------
-- 4. TESTES DE ESTRUTURAS CONDICIONAIS (IF-THEN-ELSE)
--------------------------------------------------------------------------------

testesCondicionais :: Test
testesCondicionais = TestList
    [ TestLabel "If-Then-Else Ramo True" $ TestCase $
        assertEqual "if 5 > 2 then 10 else 0" (Right (VInt 10)) (evalExprStr "if 5 > 2 then 10 else 0")

    , TestLabel "If-Then-Else Ramo False" $ TestCase $
        assertEqual "if 1 == 2 then 10 else 0" (Right (VInt 0)) (evalExprStr "if 1 == 2 then 10 else 0")
    ]

--------------------------------------------------------------------------------
-- 5. TESTES DE ESCOPO LOCAL (LET)
--------------------------------------------------------------------------------

testesLet :: Test
testesLet = TestList
    [ TestLabel "Let Simples" $ TestCase $
        assertEqual "let a = 5; in a * 2" (Right (VInt 10)) (evalExprStr "let a = 5; in a * 2")

    , TestLabel "Let com Múltiplas Atribuições" $ TestCase $
        assertEqual "let x = 10; y = 20; in x + y" (Right (VInt 30)) (evalExprStr "let x = 10; y = 20; in x + y")
    ]

--------------------------------------------------------------------------------
-- 6. TESTES DE FUNÇÕES E CLAUSURAS
--------------------------------------------------------------------------------

testesFuncoes :: Test
testesFuncoes = TestList
    [ TestLabel "Definição e Chamada de Função" $ TestCase $
        case evalProgramStr "soma x y = x + y\n soma(3, 4)" of
            Right (val, _) -> assertEqual "Resultado da soma" (VInt 7) val
            Left err       -> assertFailure ("Erro inesperado: " ++ err)

    , TestLabel "Função com Escopo e Operador Composto" $ TestCase $
        case evalProgramStr "dobro x = x * 2\n dobro(5)" of
            Right (val, _) -> assertEqual "Resultado de dobro(5)" (VInt 10) val
            Left err       -> assertFailure ("Erro inesperado: " ++ err)
    ]

--------------------------------------------------------------------------------
-- 7. TESTES DE EXECUÇÃO MULTILINHA E ESTADO (evalProgram)
--------------------------------------------------------------------------------

testesMultilinha :: Test
testesMultilinha = TestList
    [ TestLabel "Acúmulo de Estado em Memória" $ TestCase $
        let code = "x = 10\n y = x + 5\n y * 2"
        in case evalProgramStr code of
            Right (val, env) -> do
                assertEqual "Resultado da última expressão" (VInt 30) val
                assertEqual "Variável x mantida" (Just (VInt 10)) (Map.lookup "x" env)
                assertEqual "Variável y mantida" (Just (VInt 15)) (Map.lookup "y" env)
            Left err -> assertFailure ("Erro na execução multilinha: " ++ err)

    , TestLabel "Operadores Compostos Atribuição (+=)" $ TestCase $
        let code = "x = 5\n x += 10\n x"
        in case evalProgramStr code of
            Right (val, _) -> assertEqual "x após += 10" (VInt 15) val
            Left err       -> assertFailure ("Erro no operador +=: " ++ err)
    ]

--------------------------------------------------------------------------------
-- 8. TESTES DE ERROS EM TEMPO DE EXECUÇÃO (RUNTIME ERRORS)
--------------------------------------------------------------------------------

testesErrosRuntime :: Test
testesErrosRuntime = TestList
    [ TestLabel "Erro: Divisão por Zero" $ TestCase $
        case evalExprStr "10 / 0" of
            Left err -> assertBool "Capturou divisão por zero" True
            Right _  -> assertFailure "Deveria ter falhado por divisão por zero"

    , TestLabel "Erro: Variável Não Declarada" $ TestCase $
        case evalExprStr "variavelInexistente + 1" of
            Left _  -> assertBool "Capturou variável não encontrada" True
            Right _ -> assertFailure "Deveria ter falhado por variável não declarada"

    , TestLabel "Erro: Argumentos Incorretos em Função" $ TestCase $
        case evalProgramStr "f x = x + 1\n f(1, 2)" of
            Left _  -> assertBool "Capturou aridade incorreta" True
            Right _ -> assertFailure "Deveria ter falhado por número de argumentos incompatível"
    ]

--------------------------------------------------------------------------------
-- EXECUÇÃO PRINCIPAL
--------------------------------------------------------------------------------

todosTestes :: Test
todosTestes = TestList
    [ testesLiterais
    , testesOperadoresAritmeticos
    , testesListas
    , testesCondicionais
    , testesLet
    , testesFuncoes
    , testesMultilinha
    , testesErrosRuntime
    ]

main :: IO ()
main = do
    putStrLn "\n=== Executando Testes de Semântica Operacional (Interpretador) ==="
    _ <- runTestTT todosTestes
    return ()