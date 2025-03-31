-- =============================================================================
-- Parser for the Lox language, which takes a list of tokens from the Scanner and
-- builds a parse tree from them. The parse tree is a list of declarations, which
-- can be either function, variable, or statement declarations.
-- =============================================================================
module Parser (parse, runParserFromFile) where
import Debug.Trace (trace)
import Scanner (scanTokens)
import Tokens
import ParseTypes

-- Run the scanner and parser from a file as input
runParserFromFile :: FilePath -> IO ()
runParserFromFile filename = do
    content <- readFile filename
    let tokens = scanTokens content
    --putStrLn $ "Tokens: " ++ show tokens
    let parseTree = parse tokens
    print parseTree


-- Parse a list of declarations
parse :: [Token] -> ParseTree
parse [] = ParseTree([])
parse tokens = case parseDeclarations tokens of
    -- No more tokens left, return the built parse tree
    (declarations, []) -> ParseTree(declarations) 
    (_, remaining) -> error ("Unexpected tokens remaining: " ++ show remaining)

-- ================================== Declarations =====================================
-- Parse declarations until no more tokens are left
parseDeclarations :: [Token] -> ([Declaration], [Token])
parseDeclarations tokens =
    case tokens of
        [] -> error "Did not reach EOF"
        -- Stop parsing at EOF
        (TOKEN EOF _ _ _: rest) -> ([], rest)
        --  Parse single declarations until EOF is reached
        _  -> let (declaration, rest) = parseDeclaration tokens
                  (declarations, finalRest) = parseDeclarations rest
              in (declaration : declarations, finalRest)

-- Parse a single declaration
parseDeclaration :: [Token] -> (Declaration, [Token])
parseDeclaration tokens =
--    trace ("Parsing declaration with tokens: " ++ show tokens) $
    case tokens of
        -- Check if we have a function or a variable declaration
        (TOKEN FUN _ _ _ : rest) -> parseFunDeclaration rest
        (TOKEN VAR _ _ _ : rest) -> parseVarDeclaration rest
        -- Else parse a statement
        _ -> let (stmt, rest) = parseStatement tokens
             in (Statement stmt, rest)

-- Parse a function declaration
parseFunDeclaration :: [Token] -> (Declaration, [Token])
parseFunDeclaration tokens =
    case tokens of
        (token@(TOKEN IDENTIFIER name _ _) : TOKEN LEFT_PAREN _ _ _ : TOKEN RIGHT_PAREN _ _ _ : rest) ->
            case rest of
                (TOKEN LEFT_BRACE _ _ _ : rest2) ->
                    let (body, rest3) = parseBlock rest2
                    in (FunDecl token Nothing body, rest3)
                (TOKEN _ _ _ n : _ ) -> error ("Expected '{' after function parameters on line: " ++ show n)
                [] -> error "Unexpected end of input, expected '{'"
        (token@(TOKEN IDENTIFIER name _ _) : TOKEN LEFT_PAREN _ _ _ : rest) ->
            let (params, remaining) = parseParams rest
            in case remaining of
                (TOKEN LEFT_BRACE _ _ _ : rest2) ->
                    let (body, rest3) = parseBlock rest2
                    in (FunDecl token (Just params) body, rest3)
                (TOKEN _ _ _ n : _ ) -> error ("Expected '{' after function parameters on line: " ++ show n)
                [] -> error "Unexpected end of input, expected '{'"
        (TOKEN _ _ _ n : _ ) -> error ("Invalid function declaration syntax on line: " ++ show n)
        [] -> error "Unexpected end of input in function declaration"

-- Parse function parameters
parseParams :: [Token] -> ([Token], [Token])
parseParams tokens = 
    case tokens of
        -- Reached rightparen return
        (TOKEN RIGHT_PAREN _ _ _: rest) -> ([], rest)
        -- Add the token to the param list
        (token@(TOKEN IDENTIFIER _ _ _): rest) -> 
            let (params, remaining) = parseParams rest
            in (token : params, remaining)
        -- Skip commas
        (TOKEN COMMA _ _ _: rest) -> parseParams rest
        _ -> error "Invalid function parameter syntax"

-- Parse a variable declaration
parseVarDeclaration :: [Token] -> (Declaration, [Token])
parseVarDeclaration tokens =
    case tokens of
        (token@(TOKEN IDENTIFIER _ _ _) : TOKEN EQUAL _ _ _: rest) ->
            let (expr, remaining) = parseExpression rest
            in case remaining of
                (TOKEN SEMICOLON _ _ _: restAfterSemicolon) -> (VarDecl token (Just expr), restAfterSemicolon)
                _ -> error "Expected ';' after variable declaration"
        (token@(TOKEN IDENTIFIER _ _ _) : TOKEN SEMICOLON _ _ _: rest) -> (VarDecl token Nothing, rest)
        _ -> error "Invalid variable declaration syntax"

-- ============================= Statements ===============================================
-- Parse a statement
parseStatement :: [Token] -> (Stmt, [Token])
parseStatement tokens =
  --  trace ("Parsing statement with tokens: " ++ show tokens) $
    case tokens of
        (TOKEN PRINT _ _ _ : rest) -> parsePrintStatement rest
        (TOKEN RETURN _ _ _ :TOKEN SEMICOLON _ _ _ :rest) -> (Return Nothing, rest)
        (TOKEN RETURN _ _ n : rest) -> 
            case rest of
                [] -> error ("Expected statement or semicolon after return on line: " ++ show n)
                [TOKEN EOF _ _ _] -> error ("Expected statement or semicolon after return on line: " ++ show n)
                _ ->
                    let (expr, remaining) = parseExpression rest 
                    in case remaining of
                        (TOKEN SEMICOLON _ _ _ : rest2) -> (Return (Just expr), rest2)
                        (TOKEN _ _ _ line:_) -> error("Missing semicolon at end of statement at line: " ++ show remaining)
        (TOKEN IF _ _ _ : rest) -> case rest of
                                       (TOKEN LEFT_PAREN _ _ _ :rest1) -> parseIfStmt rest1
                                       (TOKEN _ _ _ line:_)            -> error("Error at line: " ++ show line)
        (TOKEN WHILE _ _ _ : rest) -> case rest of
                                        (TOKEN LEFT_PAREN _ _ _ : rest1) -> parseWhileStmt rest1
                                        (TOKEN _ _ _ line:_)            -> error("Expected opening paren at line: " ++ show line)
        (TOKEN FOR _ _ _ : rest) -> case rest of
                                        (TOKEN LEFT_PAREN _ _ _ : rest1) -> parseForStmt rest1
                                        (TOKEN _ _ _ line:_)            -> error("Expected opening paren at line: " ++ show line)
        (TOKEN LEFT_BRACE _ _ _ : rest) -> parseBlock rest 

        _ -> parseExpressionStatement tokens

-- Parse a print statement
parsePrintStatement :: [Token] -> (Stmt, [Token])
parsePrintStatement tokens =
    let (expr, rest) = parseExpression tokens
    in case rest of
        (TOKEN SEMICOLON _ _ _: remaining) -> (Print expr, remaining)
        _ -> error "Expected ';' after print statement."

-- Parse an if statement
parseIfStmt :: [Token] -> (Stmt, [Token])
parseIfStmt tokens =
    let (cond, rest1) = parseExpression tokens
    in case rest1 of
        -- Right paren should be followed by the statement
        (TOKEN RIGHT_PAREN _ _ n : rest2) ->
            case rest2 of
                [] -> error ("Expected statement after 'if' condition: " ++ show n)
                [TOKEN EOF _ _ _] -> error ("Expected statement after 'if' condition: " ++ show n)
                _ ->
                    -- Parse the statement after the right paren
                    let (thenStmt, rest3) = parseStatement rest2
                    in case rest3 of
                        -- Check if there is an else statement
                        (TOKEN ELSE _ _ _ : rest4) ->
                            let (elseStmt, rest5) = parseStatement rest4
                            in (If cond thenStmt (Just elseStmt), rest5)
                        _ -> (If cond thenStmt Nothing, rest3)
        (TOKEN _ _ _ line : _) ->
            error ("Expected ')' after if condition: " ++ show line)
        [] ->
            error "Unexpected end of input, expected ')' after if condition"

-- Parse a while statement
parseWhileStmt :: [Token] -> (Stmt, [Token])
parseWhileStmt tokens = 
    let (cond, rest) = parseExpression tokens
    in case rest of
        -- Right paren should be followed by the statement
        (TOKEN RIGHT_PAREN _ _ _ : rest2) ->
            -- Parse the statement after the right paren
            let (statement, rest3) = parseStatement rest2
            in (While cond statement, rest3)
        (TOKEN _ _ _ line : _) ->
            error ("Expected ')' after while condition" ++ show line)
        [] ->
            error "Unexpected end of input, expected ')' after while condition"

-- Parse a for statement
parseForStmt :: [Token] -> (Stmt, [Token])
parseForStmt tokens =
    let (init, rest2) = case tokens of
            (TOKEN VAR _ _ _ : initRest) ->
                let (varDecl, varRest) = parseVarDeclaration initRest
                in (Just varDecl, varRest)
            (TOKEN SEMICOLON _ _ _ : initRest) ->
                (Nothing, initRest)
            _ ->
                let (expr, exprRest) = parseExpression tokens
                in case exprRest of
                    (TOKEN SEMICOLON _ _ _ : exprRest1) ->
                        (Just (Statement (Expression expr)), exprRest1)
                    [] ->
                        error "Unexpected end of input, expected ';' after initializer"

        (cond, rest3) = case rest2 of
            (TOKEN SEMICOLON _ _ _ : condRest) ->
                (Nothing, condRest)
            _ ->
                let (expr, condRest) = parseExpression rest2
                in case condRest of
                    (TOKEN SEMICOLON _ _ _ : condRest1) ->
                        (Just expr, condRest1)
                    [] ->
                        error "Unexpected end of input, expected ';' after condition"

        (update, rest4) = case rest3 of
            (TOKEN RIGHT_PAREN _ _ _ : updateRest) ->
                (Nothing, updateRest)
            _ ->
                let (expr, updateRest) = parseExpression rest3
                in case updateRest of
                    (TOKEN RIGHT_PAREN _ _ _ : updateRest1) ->
                        (Just expr, updateRest1)
                    [] ->
                        error "Unexpected end of input, expected ')' after increment"

    -- The rest will be parsed as the body
    in case rest4 of
        (t:_) ->
            let (body, rest5) = parseStatement rest4
            in (For init cond update body, rest5)
        [] ->
            error "Unexpected end of input, expected body"


-- Parse a block of statements
parseBlock :: [Token] -> (Stmt, [Token])
parseBlock (TOKEN EOF _ _ n : _) = error ("Missing right brace at end of block on line: " ++ show n)
parseBlock (TOKEN RIGHT_BRACE _ _ _ : rest) = (Block [], rest)
parseBlock tokens@(TOKEN _ _ _ n : _) =
    let (decl, rest) = parseDeclaration tokens
        (Block decls, rest2) = parseBlock rest
    in (Block (decl : decls), rest2)
parseBlock [] = error "Unexpected end of input in block"

-- Parse an expression statement
parseExpressionStatement :: [Token] -> (Stmt, [Token])
parseExpressionStatement tokens =
    case tokens of
        (TOKEN SEMICOLON _ _ n: rest) -> error ("Empty statement on line: " ++ show n)
        _ ->
            let (expr, rest) = parseExpression tokens
            in case rest of
                (TOKEN SEMICOLON _ _ _ : remaining) -> (Expression expr, remaining)
                _ -> error ("Expected ';' after expression, remaining: " ++ show rest)

-- =================================== Expression ========================================
parseExpression :: [Token] -> (Expr, [Token])
parseExpression tokens = parseAssignment tokens
-- Parse an assignment, right associative
parseAssignment :: [Token] -> (Expr, [Token])
parseAssignment tokens =
  let (left, rest) = parseLogicOr tokens
  in case rest of
       (TOKEN EQUAL _ _ n : rest1) -> 
         case left of
         -- Check that left hand side is a variable
            Variable var -> 
                let (right, rest2) = parseAssignment rest1
                in (Assign var right, rest2)
            _ -> error ("Invalid assignment target on line: "++ show n)
       _ -> (left, rest)

-- ========= All the following binary operators are parsed in the same way ===================
-- Parse the logic or, left associative
parseLogicOr :: [Token] -> (Expr, [Token])
parseLogicOr tokens =
    let (left, rest) = parseLogicAnd tokens
    in parseLogicOrRest left rest
-- Helper function for left associativity
parseLogicOrRest :: Expr -> [Token] -> (Expr, [Token])
parseLogicOrRest left tokens =
    case tokens of
        (token@(TOKEN OR _ _ _) : rest1) ->
            -- Parse the right side of the expression
            let (right, rest2) = parseLogicAnd rest1
                newLeft = Binary left token right
                -- Continue parsing the right side
            in parseLogicOrRest newLeft rest2
            -- No more OR tokens, return the left side
        _ -> (left, tokens)
        
parseLogicAnd :: [Token] -> (Expr, [Token])
parseLogicAnd tokens =
    let (left, rest) = parseEquality tokens
    in parseLogicAndRest left rest
parseLogicAndRest :: Expr -> [Token] -> (Expr, [Token])
parseLogicAndRest left tokens =
    case tokens of
        (token@(TOKEN AND _ _ _) : rest1) ->
            let (right, rest2) = parseEquality rest1
                newLeft = Binary left token right
            in parseLogicAndRest newLeft rest2
        _ -> (left, tokens)

parseEquality :: [Token] -> (Expr, [Token])
parseEquality tokens =
    let (left, rest) = parseComparison tokens
    in parseEqualityRest left rest
parseEqualityRest :: Expr -> [Token] -> (Expr, [Token])
parseEqualityRest left tokens =
    case tokens of
        (token@(TOKEN t _ _ _) : rest1) | t `elem` [EQUAL_EQUAL, BANG_EQUAL] ->
            let (right, rest2) = parseComparison rest1
                newLeft = Binary left token right
            in parseEqualityRest newLeft rest2
        _ -> (left, tokens)

parseComparison :: [Token] -> (Expr, [Token])
parseComparison tokens =
    let (left, rest) = parseTerm tokens
    in parseComparisonRest left rest
parseComparisonRest :: Expr -> [Token] -> (Expr, [Token])
parseComparisonRest left tokens =
    case tokens of
        (token@(TOKEN t _ _ _) : rest1) | t `elem` [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] ->
            let (right, rest2) = parseTerm rest1
                newLeft = Binary left token right
            in parseComparisonRest newLeft rest2
        _ -> (left, tokens)

parseTerm :: [Token] -> (Expr, [Token])
parseTerm tokens =
    let (left, rest) = parseFactor tokens
    in parseTermRest left rest
parseTermRest :: Expr -> [Token] -> (Expr, [Token])
parseTermRest left tokens =
    case tokens of
        (token@(TOKEN t _ _ _) : rest1) | t `elem` [PLUS, MINUS] ->
            let (right, rest2) = parseFactor rest1
                newLeft = Binary left token right
            in parseTermRest newLeft rest2
        _ -> (left, tokens)

parseFactor :: [Token] -> (Expr, [Token])
parseFactor tokens =
    let (left, rest) = parseUnary tokens
    in parseFactorRest left rest
parseFactorRest :: Expr -> [Token] -> (Expr, [Token])
parseFactorRest left tokens =
    case tokens of
        (token@(TOKEN t _ _ _) : rest1) | t `elem` [STAR, SLASH] ->
            let (right, rest2) = parseUnary rest1
                newLeft = Binary left token right
            in parseFactorRest newLeft rest2
        _ -> (left, tokens)

-- Parse unary operators, this is right associative
parseUnary :: [Token] -> (Expr, [Token])
parseUnary tokens =
    case tokens of
        (token@(TOKEN MINUS _ _ _) : rest) -> 
            let (right, remaining) = parseUnary rest
            in (Unary token right, remaining)

        (token@(TOKEN BANG _ _ _) : rest) -> 
            let (right, remaining) = parseUnary rest
            in (Unary token right, remaining)

        _ ->
            let (primary, rest1) = parsePrimary tokens
            in parseCall primary rest1

parseCall :: Expr -> [Token] -> (Expr, [Token])
parseCall expr tokens =
    case tokens of
        (TOKEN LEFT_PAREN _ _ _ : rest) ->
            let (args, remaining) = parseArguments rest
            in case remaining of
                (token@(TOKEN RIGHT_PAREN _ _ _) : rest2) ->
                    parseCall (Call expr token args) rest2
                (TOKEN _ _ _ n :_) -> error ("Expected ')' after arguments on line: " ++ show n)
                [] -> error "Unexpected end of input, expected ')'"
        _ -> (expr, tokens)

-- Parse the arguments of the function call
parseArguments :: [Token] -> ([Expr], [Token])
parseArguments tokens =
    case tokens of
        -- Right paren mean no arguments, return
        (TOKEN RIGHT_PAREN _ _ _ : rest) -> ([], tokens)
        _ ->
            let (arg, rest1) = parseExpression tokens
                (args, remaining) = case rest1 of
                    -- check for the comma and continue until right paren is reached :w
                    (TOKEN COMMA _ _ _ : restNext) -> parseArguments restNext
                    _ -> ([], rest1)
            in (arg : args, remaining)

-- Parse the primary token
parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary tokens =
    case tokens of
        -- Create the literals
        (TOKEN NUMBER _ n _: rest) -> (Literal n, rest)
        (TOKEN STRING _ s _: rest) -> (Literal (s), rest)
        (TOKEN TRUE _ _ _: rest) -> (Literal TRUE_LIT, rest)
        (TOKEN FALSE _ _ _: rest) -> (Literal FALSE_LIT, rest)
        (TOKEN NIL _ _ _: rest) -> (Literal NIL_LIT, rest)
        -- Semicolon reached, continue with next
        (TOKEN SEMICOLON _ _ _: rest) -> parseExpression rest
        -- Create a variable if it is an identifier
        (token@(TOKEN IDENTIFIER name _ _): rest) -> (Variable token, rest)
        -- Create a grouping if it begins with "(" and ends with ")"
        (TOKEN LEFT_PAREN _ _ _: rest) ->
            let (expr, remaining) = parseExpression rest
            in case remaining of
                (TOKEN RIGHT_PAREN _ _ _ : rest2) -> (Grouping expr, rest2)
                _ -> error "Expected ')' after expression"
        [] -> error "Unexpected end of input"
        ((TOKEN RIGHT_BRACE _ _ _) : (TOKEN EOF _ _ _):_) -> error "Unexpected right brace"
        (TOKEN _ s _ n:_) -> error ( "Unexpected token: " ++ show s ++ " on line: " ++ show n)
