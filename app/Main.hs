module Main where

import Parser
import AST
import Text.Megaparsec
import Test.HUnit
import Data.Either (isRight, isLeft)
import System.Exit (exitFailure, exitSuccess)

-- | Função auxiliar para parsear uma única linha.
-- Usamos `sc *> pStatement <* eof` para garantir que ele leia a linha toda
-- e não deixe lixo (ou caracteres inválidos) para trás.
parseLine :: String -> Either String Statement
parseLine input = case parse (sc *> pStatement <* eof) "<test>" input of
    Left err -> Left (errorBundlePretty err)
    Right ast -> Right ast

--------------------------------------------------------------------------------
-- 1. TESTES DE IGUALDADE
--------------------------------------------------------------------------------
testesAST :: Test
testesAST = TestLabel "Testes de AST Exata" $ TestList
    [ TestCase $ assertEqual "Parse Inteiro" 
        (Right (ExprStatement (LitInt 42))) (parseLine "42")
        
    , TestCase $ assertEqual "Parse Float" 
        (Right (ExprStatement (LitFloat 3.14))) (parseLine "3.14")
        
    , TestCase $ assertEqual "Precedencia: 2 + 3 * 4" 
        (Right (ExprStatement (BinaryOp "+" (LitInt 2) (BinaryOp "*" (LitInt 3) (LitInt 4))))) 
        (parseLine "2 + 3 * 4")
        
    , TestCase $ assertEqual "Anotacao de Funcao" 
        (Right (DeclarationStatement (FuncAnnot "somaLista" ["[Int]", "Int"]))) 
        (parseLine "somaLista :: [Int] -> Int")
        
    , TestCase $ assertEqual "Atribuicao Composta" 
        (Right (DeclarationStatement (Assignment "x" "+=" (BinaryOp "*" (LitInt 12) (LitInt 3))))) 
        (parseLine "x += 12 * 3")
    ]
--------------------------------------------------------------------------------
-- 2. CASOS DE SUCESSO (Retirados do inputs.txt)
--------------------------------------------------------------------------------
casosSucesso :: [String]
casosSucesso = 
    [ "true"
    , "false"
    , "x"
    , "(2 + 3) * 4"
    , "[1, 2, 3]"
    , "true && 12"
    , "(false && true) || true"
    , "12 % 3"
    , "\"teste\""
    , "x = x + y"
    , "x == x"
    , "123 + 3 -- adasdklasd"     -- Testa se o consumidor (sc) lida com comentário no final
    , "3.14 + 2 + 1.23"
    , "\"asm,dad\" + \"askjdw\""
    , "-12"
    , "concatenation :: [String] -> String"
    , "3 ^ 2"
    , "cos (x + 1)"
    , "pow (2, 3)"
    ]

testesSucesso :: Test
testesSucesso = TestLabel "Casos de Sucesso" $TestList$ map criarTesteSucesso casosSucesso
  where
    criarTesteSucesso input = TestCase $ 
        assertBool ("Deveria parsear com sucesso: " ++ input) (isRight $ parseLine input)

--------------------------------------------------------------------------------
-- 3. CASOS DE FALHA SINTÁTICA (Retirados do inputs.txt)
--------------------------------------------------------------------------------
casosFalha :: [String]
casosFalha = 
    [ "sin x"         -- Faltam parênteses na chamada de função (exigido pelo seu pCallFunction)
    , "2 +"           -- Operador sem termo à direita
    , "(2 + 3"        -- Parêntese aberto, mas não fechado
    , "[1, 2"         -- Lista não fechada
    , "x ="           -- Atribuição sem valor
    , "123abc"        -- Identificador inválido (número colado em letra, pInteger vai barrar)
    , "foo(1,)"       -- Vírgula sobrando nos argumentos
    , "....,,,,"      -- Símbolos aleatórios
    , "123_abc"       -- Número colado em underscore
    , "3.14abc"       -- Float colado em letra
    , "2a + 3"        -- Variável iniciando com número em vez de letra
    , "12 = 3 + 1"    -- Tentando atribuir a um literal (pAssignment exige pIdentifier na esquerda)
    , "asse@ = \"t\"" -- Caractere inválido (@) no nome da variável
    ]

testesFalha :: Test
testesFalha = TestLabel "Casos de Erro Sintático" $TestList$ map criarTesteFalha casosFalha
  where
    criarTesteFalha input = TestCase $ 
        assertBool ("Deveria falhar ao parsear: " ++ input) (isLeft $ parseLine input)

--------------------------------------------------------------------------------
-- EXECUÇÃO
--------------------------------------------------------------------------------
todosTestes :: Test
todosTestes = TestList [testesAST, testesSucesso, testesFalha]

main :: IO ()
main = do
    putStrLn "\nIniciando bateria de testes com HUnit...\n"
    contagens <- runTestTT todosTestes
    
    -- Se houver falhas ou erros, quebra o build (retorna exitFailure para o terminal)
    if errors contagens + failures contagens == 0
        then do
            putStrLn "\nTodos os testes passaram com sucesso!"
            exitSuccess
        else do
            putStrLn "\nAlguns testes falharam. Verifique os relatórios acima."
            exitFailure