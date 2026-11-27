module Main where

import Parser
import Text.Megaparsec (parse, errorBundlePretty)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
    input <- getContents

    case parse pProgram "<stdin>" input of
        Left err -> do
            hPutStrLn stderr (errorBundlePretty err)
            exitFailure

        Right ast -> pPrint ast