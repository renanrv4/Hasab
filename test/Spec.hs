module Main where

import Parser
import AST
import Text.Megaparsec (parse, errorBundlePretty, eof)
import Test.HUnit
import Data.Either (isRight, isLeft)
import System.Exit (exitFailure, exitSuccess)

--------------------------------------------------------------------------------
-- 1. FUNCOES AUXILIARES
--------------------------------------------------------------------------------

-- Analisa uma unica instrucao e exige que toda a entrada seja consumida.
parseLine :: String -> Either String Statement
parseLine input =
    case parse (sc *> pStatement <* eof) "<test>" input of
        Left err  -> Left (errorBundlePretty err)
        Right ast -> Right ast

-- Analisa um programa completo.
-- Usado, por exemplo, para testar arquivos que contem apenas comentarios.
parseSource :: String -> Either String Program
parseSource input =
    case parse pProgram "<test>" input of
        Left err  -> Left (errorBundlePretty err)
        Right ast -> Right ast

--------------------------------------------------------------------------------
-- 2. TESTES DE AST EXATA
--------------------------------------------------------------------------------

testesAST :: Test
testesAST = TestLabel "Testes de AST Exata" $ TestList
    [ testAST "42"
        (ExprStatement (LitInt 42))

    , testAST "3.14"
        (ExprStatement (LitFloat 3.14))

    , testAST "2 + 3 * 4"
        (ExprStatement
            (BinaryOp "+" (LitInt 2)
                (BinaryOp "*" (LitInt 3) (LitInt 4))))

    , testAST "(2 + 3) * 4"
        (ExprStatement
            (BinaryOp "*"
                (BinaryOp "+" (LitInt 2) (LitInt 3))
                (LitInt 4)))

    , testAST "[[1, 2], [3, 1]]"
        (ExprStatement
            (ListExpr
                [ ListExpr [LitInt 1, LitInt 2]
                , ListExpr [LitInt 3, LitInt 1]
                ]))

    , testAST "3 ^ 2 ^ 4"
        (ExprStatement
            (BinaryOp "^" (LitInt 3)
                (BinaryOp "^" (LitInt 2) (LitInt 4))))

    , testAST "[1] ++ [2] ++ [3]"
        (ExprStatement
            (BinaryOp "++"
                (ListExpr [LitInt 1])
                (BinaryOp "++"
                    (ListExpr [LitInt 2])
                    (ListExpr [LitInt 3]))))

    , testAST "somaLista :: [Int] -> Int"
        (DeclarationStatement
            (FuncAnnot "somaLista" ["[Int]", "Int"]))

    , testAST "x += 12 * 3"
        (DeclarationStatement
            (Assignment "x" "+="
                (BinaryOp "*" (LitInt 12) (LitInt 3))))

    , testAST "distancia x y = sqrt(x^2 + y^2)"
        (DeclarationStatement
            (FuncDef "distancia" ["x", "y"]
                (Call "sqrt"
                    [ BinaryOp "+"
                        (BinaryOp "^" (Var "x") (LitInt 2))
                        (BinaryOp "^" (Var "y") (LitInt 2))
                    ])))

    , testAST "if (x > 0) then x else 0"
        (ExprStatement
            (IfThenElse
                (BinaryOp ">" (Var "x") (LitInt 0))
                (Var "x")
                (LitInt 0)))
            
    , testAST "if (x > 0) then x + 1 else x - 1"
        (ExprStatement
            (IfThenElse
                (BinaryOp ">" (Var "x") (LitInt 0))
                    (BinaryOp "+" (Var "x") (LitInt 1))
                    (BinaryOp "-" (Var "x") (LitInt 1))))
                    
    , testAST "let a = 2; s = 3 in a + s"
        (ExprStatement
            (Let
                [ Assignment "a" "=" (LitInt 2)
                , Assignment "s" "=" (LitInt 3)
                ]
                (BinaryOp "+" (Var "a") (Var "s"))))

    , testAST "f x = let a = x + 1; b = a * 2 in b"
        (DeclarationStatement
            (FuncDef "f" ["x"]
                (Let
                    [ Assignment "a" "="
                        (BinaryOp "+" (Var "x") (LitInt 1))
                    , Assignment "b" "="
                        (BinaryOp "*" (Var "a") (LitInt 2))
                    ]
                    (Var "b"))))
    ]

testAST :: String -> Statement -> Test
testAST input expected = TestCase $
    assertEqual
        ("AST incorreta para: " ++ input)
        (Right expected)
        (parseLine input)

--------------------------------------------------------------------------------
-- 3. CASOS QUE DEVEM SER ACEITOS
--------------------------------------------------------------------------------

casosSucesso :: [String]
casosSucesso =
    [ "42"
    , "3.14"
    , "true"
    , "false"
    , "x"
    , "x + 1"
    , "2 + 3 * 4"
    , "(2 + 3) * 4"
    , "[1, 2, 3]"
    , "[[1, 2], [3, 1]]"
    , "true && 12"
    , "(false && true) || true"
    , "12 % 3"
    , "\"teste\""
    , "x += 12 * 3"
    , "x = x + y"
    , "x == x"
    , "123 + 3 -- adasdklasd"
    , "3.14 + 2 + 1.23"
    , "\"asm,dad\" + \"askjdw\""
    , "-12"
    , "somaLista :: [Int] -> Int"
    , "concatenation :: [String] -> String"
    , "multFloat :: Float -> Float"
    , "3 ^ 2"
    , "3 * 2 * 4"
    , "3 ^ 2 ^ 4"
    , "cos (x + 1)"
    , "sqrt (x^2 + y^2)"
    , "pow (2, 3)"
    , "distancia :: Float -> Float -> Float"
    , "distancia x y = sqrt(x^2 + y^2)"
    , "[1] ++ [2] ++ [3]"
    , "1 : [2, 3]"
    , "[1, 2] ++ [3, 4]"
    , "1 : 2 : []"
    , "x ++ y"
    , "f :: Int -> Int"
    , "let a = 2; s = 3 in a + s"
    , "f x = let a = x + 1; b = a * 2 in b"
    , "if (x > 0) then x else 0"
    , "if true then 1 else 0"
    , "f x = if x > 0 then x + 1 else x - 1"
    ]

testesSucesso :: Test
testesSucesso = TestLabel "Casos de Sucesso" $
    TestList (map criarTesteSucesso casosSucesso)
  where
    criarTesteSucesso input = TestCase $
        assertBool
            ("Deveria aceitar: " ++ input)
            (isRight (parseLine input))

-- Um comentario isolado nao produz uma instrucao.
testeComentarioIsolado :: Test
testeComentarioIsolado = TestCase $
    assertEqual "Comentario isolado gera programa vazio"
        (Right [])
        (parseSource "--comentario")

-- Um programa vazio tambem deve ser aceito.
testeProgramaVazio :: Test
testeProgramaVazio = TestCase $
    assertEqual "Programa vazio"
        (Right [])
        (parseSource "")

--------------------------------------------------------------------------------
-- 4. CASOS QUE DEVEM SER REJEITADOS
--------------------------------------------------------------------------------

casosFalha :: [String]
casosFalha =
    [ "sqrt x^2 + y^2"
    , "sin x"
    , "sqrt x"
    , "log x"
    , "2 +"
    , "(2 + 3"
    , "[1, 2"
    , "x ="
    , "123abc"
    , "foo(1,)"
    , "foo(,1)"
    , "....,,,,"
    , "3 ** 2"
    , "123_abc"
    , "123abs_"
    , "3.14abc"
    , "2a + 3"
    , "123x = 3"
    , "123 x = 3"
    , "12 = 3 + 1"
    , "asse@ = \"teste\""
    , "if x > 0 thne x else -x"
    ]

testesFalha :: Test
testesFalha = TestLabel "Casos de Falha Sintatica" $
    TestList (map criarTesteFalha casosFalha)
  where
    criarTesteFalha input = TestCase $
        assertBool
            ("Deveria rejeitar: " ++ input)
            (isLeft (parseLine input))

--------------------------------------------------------------------------------
-- 5. EXECUCAO
--------------------------------------------------------------------------------

todosTestes :: Test
todosTestes = TestList
    [ testesAST
    , testesSucesso
    , testeComentarioIsolado
    , testeProgramaVazio
    , testesFalha
    ]

main :: IO ()
main = do
    putStrLn "\nIniciando bateria de testes com HUnit...\n"

    contagens <- runTestTT todosTestes

    if errors contagens + failures contagens == 0
        then do
            putStrLn "\nTodos os novos testes passaram com sucesso!"
            exitSuccess
        else do
            putStrLn "\nAlguns testes falharam. Verifique os relatorios acima."
            exitFailure
