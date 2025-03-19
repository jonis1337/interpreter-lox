-- =============================================================================
-- Interpreter for the lox launguage, the program will use the lexer, parser and
-- the interpretator to run the lox program. The program will read a .lox file from
-- the command line then the program will use the scanner to tokenize the input. The
-- list of tokens will then be passed to the parser that will check the syntax of the
-- input program that got tokenized. The parser is left recursive and will build a
-- parse tree. The parse tree will then be passed to the interpretator that will
-- evaluate the parse tree. The interpretator will keep track of the environment and
-- output. The environment is a map of variable names to values. The output is a list
-- of strings that will be printed at the end of the program.
-- =============================================================================
module Main where 

import System.Environment (getArgs)
import Parser (parse)
import Scanner (scanTokens)
import Interpretator (interpreter, State(..))

import qualified Data.Map as Map

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 
        then putStrLn "Usage: ./interpreter <filename>"
        else do
            let filename = head args 
            file <- readFile filename
            let tokens = scanTokens file
            let program = parse tokens
            print program
            let result = interpreter program
            -- print the result
            print result
