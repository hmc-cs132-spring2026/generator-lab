module Main
    ( main
    ) where

import           Args                   (Options (..), options)

import           Control.Monad          (when)

import           Eval                   (eval)

import           Lexer                  (tokenize)

import           Parser                 (parse)

import           System.Console.CmdArgs (cmdArgs)

main :: IO ()
main = do
    -- Get command line options
    opts <- cmdArgs options

    -- Retrieve the input string from the command line options
    let inputString = expr opts

    -- If requested, print the result of tokenization
    when (printTokens opts) $
        putStrLn $ "Tokens: " ++ show (tokenize inputString)

    -- If requested, print the result of parsing
    when (printAst opts) $
        putStrLn $ "AST: " ++ show (parse inputString)

    -- Print the evaluation result
    print (eval (parse inputString))

