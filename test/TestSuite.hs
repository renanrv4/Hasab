module Main where

import Test.HUnit

import qualified TestParser
import qualified TestType
import qualified TestInterpreter

main :: IO ()
main = do
    putStrLn "\nIniciando bateria completa de testes...\n"

    counts <- runTestTT $ TestList
        [ TestLabel "Parser" TestParser.todosTestes
        , TestLabel "Typechecker" TestType.todosTestes
        , TestLabel "Interpreter" TestInterpreter.todosTestes
        ]

    if errors counts + failures counts == 0
        then putStrLn "\nTodos os testes passaram!"
        else fail "\nAlguns testes falharam."