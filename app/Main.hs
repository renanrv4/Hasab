module Main where

import Parser
import Text.Megaparsec
import Control.Monad (forM_)

main :: IO ()
main = do
    contents <- readFile "test/inputs.txt"

    let inputs = lines contents

    forM_ inputs $ \input -> do
        putStrLn $ "Input:  " ++ input

        case parse pProgram "<test>" input of
            Left err ->
                putStrLn $ "Error:  " ++ errorBundlePretty err

            Right ast ->
                putStrLn $ "AST:    " ++ show ast

        putStrLn "--------------------"