-- =============================================================================
-- Interpretator for the lox launguage, the program will use the lexer and parser
-- to get the parse tree and then evaluate the parse tree. During evaluation the
-- program will keep track of the environment and output. The environment is a
-- map of variable names to values. The output is a list of strings that will be
-- printed at the end of the program.
-- =============================================================================
module Interpretator (
    interpreter,
    State(..),
    Value(..),
    Env
) where

import System.Environment (getArgs)
import Parser (parse)
import Scanner (scanTokens)
import Tokens
import ParseTypes
import Debug.Trace (trace)

import qualified Data.Map as Map


type Env = [Map.Map String Value]
-- == Data types ==
data Value
    = Nil
    | BoolVal Bool
    | Number Double
    | StringVal String
    deriving (Show, Eq)

data State = State {
    environment :: Env, 
    output :: [String]
    }
instance Show State where
    -- print output on new line for each element
    show (State env out) = "Environment: " ++ show env ++ "\nOutput: {\n\n" ++ unlines out ++ "\n}"

-- == Helper functions ==
toFloat :: Value -> Double
toFloat (Number n) = n
toFloat _ = error "Expected number"

-- Lox thruthiness
toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool (Number n) = True
toBool Nil = False
toBool _ = True

showValue :: Value -> String
showValue (BoolVal b) = if b then "true" else "false"
showValue (Number n) = show n
showValue (StringVal s) = s
showValue Nil = "nil"

lookupValue:: String -> Env -> Maybe Value
lookupValue name [] = Nothing
lookupValue name (env:envs) = 
    case Map.lookup name env of
        Just value -> Just value
        Nothing -> lookupValue name envs
-- Update the value of a variable in the environment
updateValue :: String -> Value -> Env -> Env
updateValue name value [] = [Map.singleton name value]
updateValue name value (scope:rest) =
    if Map.member name scope
        then Map.insert name value scope : rest
        else scope : updateValue name value rest

-- ===================== Interpreter =====================
interpreter :: ParseTree -> State
-- create the initial state and evaluate the parse tree
interpreter (ParseTree decls) = evalDeclarations decls (State {environment = [Map.empty], output = []})

-- ===================== Declaration =====================
evalDeclarations :: [Declaration] -> State -> State
evalDeclarations [] state = state
evalDeclarations (d:ds) state =
    let newState = evalDeclaration d state
    in evalDeclarations ds newState

-- Evaluate a single declaration
evalDeclaration :: Declaration -> State -> State
evalDeclaration (Statement stmt) state = evalStmt stmt state
evalDeclaration (VarDecl token (Just expr)) state =
    let name = case token of
            TOKEN IDENTIFIER n _ _ -> n
            _ -> error "Invalid variable name"
        (value, state1) = evalExpr expr state
        newEnv = case environment state1 of
            (currentScope:rest) -> Map.insert name value currentScope : rest
            [] -> [Map.singleton name value]
    in state1 {environment = newEnv}
evalDeclaration (VarDecl token Nothing) state = 
    let name = case token of
            TOKEN IDENTIFIER n _ _ -> n
            _ -> error "Invalid variable name"
        newEnv = case environment state of
            (currentScope:rest) -> Map.insert name Nil currentScope : rest
            [] -> [Map.singleton name Nil]
    in state {environment = newEnv}
evalDeclaration _ _ = error "Unimplemented declaration type"


-- ===================== Statement =====================
evalStmt :: Stmt -> State -> State
evalStmt (Expression expr) state = 
    let (_, state1) = evalExpr expr state
    in state1
evalStmt (Print expr) state = 
    let (value, state1) = evalExpr expr state
        str = showValue value
    in state1 {output = output state1 ++ [str]}
evalStmt (If expr stmt Nothing) state = 
    let (value, state1) = evalExpr expr state
    in if toBool value
       then evalStmt stmt state1
       else state1
evalStmt (If expr stmt (Just elseStmt)) state = 
    let (value, state1) = evalExpr expr state
    in if toBool value
        then evalStmt stmt state1
        else evalStmt elseStmt state1

evalStmt (Block decls) state = -- Create new scope for block
    let stateWithNewScope = state {environment = Map.empty : environment state}
        stateAfterBlock = evalDeclarations decls stateWithNewScope
    -- Return to parent scope after block
    in stateAfterBlock {environment = tail $ environment stateAfterBlock}

evalStmt (While expr stmt) state = 
    let (value, state1) = evalExpr expr state
    in if toBool value
        then let newState = evalStmt stmt state1
             in evalStmt (While expr stmt) newState
        else state1

evalStmt (For init cond incr stmt) state = 
    let state1 = case init of
            Just decl -> evalDeclaration decl state
            Nothing -> state
    in case cond of
        Just expr -> 
            let (value, state2) = evalExpr expr state1
            in if toBool value
               then let newState = evalStmt stmt state2
                        (incrValue, incrState) = case incr of
                            Just incrExpr -> evalExpr incrExpr newState
                            Nothing -> (Nil, newState)
                    in evalStmt (For Nothing cond incr stmt) incrState
               else state2
        Nothing -> 
            let newState = evalStmt stmt state1
                (incrValue, incrState) = case incr of
                    Just incrExpr -> evalExpr incrExpr newState
                    Nothing -> (Nil, newState)
            in evalStmt (For Nothing Nothing incr stmt) incrState
    
evalStmt _ _ = error "Unimplemented statement type"

-- ===================== Expression =====================
evalExpr :: Expr -> State -> (Value, State)
evalExpr (Assign var expr) state = 
    let name = case var of
            TOKEN IDENTIFIER n _ _ -> n
            _ -> error "Invalid variable name"
        (value, state1) = evalExpr expr state
        currentEnv = environment state1
    in case lookupValue name currentEnv of
        Just _ -> (value, state1 {environment = updateValue name value currentEnv})
        Nothing -> error ("Variable " ++ name ++ " not declared")
evalExpr (Variable var) state = 
    let name = case var of
            TOKEN IDENTIFIER n _ _ -> n
            _ -> error "Invalid variable name"
    in case lookupValue name (environment state) of 
           Just value -> (value, state)
           Nothing -> error ("Variable " ++ name ++ " not declared")
evalExpr (Literal l) state = 
    case l of
        NONE -> (Nil, state)
        STR s -> (StringVal s, state)
        NUM n -> (Number (realToFrac n), state)
        TRUE_LIT -> (BoolVal True, state)
        FALSE_LIT -> (BoolVal False, state)
        NIL_LIT -> (Nil, state)
        _ -> error "Invalid literal"
evalExpr (Binary l op r) state = 
    let (left, state1) = evalExpr l state
    in case op of
        TOKEN OR _ _ _ ->
            if toBool left 
                then (left, state1)
                else evalExpr r state1
        TOKEN AND _ _ _ -> 
            if toBool left
                then evalExpr r state1
                else (left, state1)
    
        _ -> 
            let (right, state2) = evalExpr r state1
            in case op of
                TOKEN PLUS _ _ n -> case (left, right) of 
                    (Number a, Number b) -> (Number (a + b), state2)
                    (StringVal a, StringVal b) -> (StringVal (a ++ b), state2)
                    _ -> error ("Types not compatible with \"+\" operator on line: " ++ show n)
                TOKEN MINUS _ _ _ -> (Number (toFloat left - toFloat right), state2)
                TOKEN STAR _ _ _ -> (Number (toFloat left * toFloat right), state2)
                TOKEN SLASH _ _ _ -> (Number (toFloat left / toFloat right), state2)
                TOKEN GREATER _ _ _ -> (BoolVal (toFloat left > toFloat right), state2)
                TOKEN GREATER_EQUAL _ _ _ -> (BoolVal (toFloat left >= toFloat right), state2)
                TOKEN LESS _ _ _ -> (BoolVal (toFloat left < toFloat right), state2)
                TOKEN LESS_EQUAL _ _ _ -> (BoolVal (toFloat left <= toFloat right), state2)
                TOKEN EQUAL_EQUAL _ _ _ -> (BoolVal (left == right), state2)
                TOKEN BANG_EQUAL _ _ _ -> (BoolVal (left /= right), state2)
                TOKEN _ d _ n -> error ("Unsupported operator on line: " ++ show n ++ show d)
evalExpr (Unary op expr) state = 
    let (value, state1) = evalExpr expr state
    in case op of
        TOKEN MINUS _ _ _ -> (Number (- toFloat value), state1)
        TOKEN BANG _ _ _ -> (BoolVal (not (toBool value)), state1)
        _ -> error "Invalid unary operator"
evalExpr (Grouping expr) state = evalExpr expr state
evalExpr _ _ = error "Unimplemented expression type"