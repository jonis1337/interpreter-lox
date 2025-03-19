-- ===================================================================
-- Scanner for Lox programming language, reads a string and transforms
-- it into and array of tokens.
-- Main function: scanTokens :: [Char] -> [Token] 
-- ===================================================================

module Scanner (scanTokens) where
import Debug.Trace (trace)
import Data.Char
import Tokens

scanTokens :: [Char] -> [Token]
scanTokens str = scanToken str 1 []

scanToken :: [Char] -> Int -> [Token] -> [Token]
scanToken [] line tokens = tokens ++ [TOKEN EOF "" NONE line]
scanToken (c:cs) line tokens
    -- Handle single char tokens
    | c == '(' = scanToken cs line (tokens ++ [TOKEN LEFT_PAREN "(" NONE line])
    | c == ')' = scanToken cs line (tokens ++ [TOKEN RIGHT_PAREN ")" NONE line])
    | c == '{' = scanToken cs line (tokens ++ [TOKEN LEFT_BRACE "{" NONE line])
    | c == '}' = scanToken cs line (tokens ++ [TOKEN RIGHT_BRACE "}" NONE line])
    | c == ',' = scanToken cs line (tokens ++ [TOKEN COMMA "," NONE line])
    | c == '.' = scanToken cs line (tokens ++ [TOKEN DOT "." NONE line])
    | c == '-' = scanToken cs line (tokens ++ [TOKEN MINUS "-" NONE line])
    | c == '+' = scanToken cs line (tokens ++ [TOKEN PLUS "+" NONE line])
    | c == ';' = scanToken cs line (tokens ++ [TOKEN SEMICOLON ";" NONE line])
    | c == '*' = scanToken cs line (tokens ++ [TOKEN STAR "*" NONE line])
    -- Handle two-char tokens
    | c == '!'  = match cs line tokens c BANG BANG_EQUAL
    | c == '='  = match cs line tokens c EQUAL EQUAL_EQUAL
    | c == '<'  = match cs line tokens c LESS LESS_EQUAL
    | c == '>'  = match cs line tokens c GREATER GREATER_EQUAL
    --Ignore comments
    | c == '/' = if take 1 cs == "/" 
        --Skip to end of line
        then skipComment cs line tokens
        --Handle single char token
        else scanToken cs line (tokens ++ [TOKEN SLASH "/" NONE line])
    -- Handle end of line
    | c == '\n' = scanToken cs (line + 1) tokens
    -- Ignore whitespace
    | c == '\r' = scanToken cs line tokens
    | c == '\t' = scanToken cs line tokens
    | isSpace c = scanToken cs line tokens
    -- Handle identifiers and keywords
    | isValidAlpha c = identifier (c:cs) line tokens
    -- Handle numbers
    | isDigit c = scanNumber (c:cs) line tokens
    -- Handle strings
    | c == '"' = string cs line tokens
    | otherwise = error ("scanner error line: " ++ show line ++ " - '" ++ [c] ++ "' is not a valid sign." )

-- Match a two-character token
match :: [Char] -> Int -> [Token] -> Char -> TokenType -> TokenType -> [Token]
match (next:rest) line tokens c currentTokenType nextTokenType = 
    if next == '=' then 
        -- Matched the two-character token, so add it to the list
        scanToken rest line (tokens ++ [TOKEN nextTokenType [c, next] NONE line])
    else
        -- Not a match, treat as a single character token
        scanToken (next:rest) line (tokens ++ [TOKEN currentTokenType [c] NONE line])
-- Handle single character token
match [] _ tokens c currentTokenType _ = tokens ++ [TOKEN currentTokenType [c] NONE 0]

skipComment :: [Char] -> Int -> [Token] -> [Token]
-- If comment is last in file call scanToken to get correct line number
skipComment [] line tokens = scanToken [] line tokens
-- Check for line break, if so increment line and call scanToken
skipComment ('\n':cs) line tokens = scanToken cs (line + 1) tokens
-- Skip to the end of line or until we have nothing left to parse
skipComment (_:cs) line tokens = skipComment cs line tokens
-- Handle keywords and identifiers
identifier :: [Char] -> Int -> [Token] -> [Token]
identifier cs line tokens = 
    let (ident, rest) = span isValidNumericAlpha cs
        tokenType = case ident of 
            "and"    -> AND
            "class"  -> CLASS
            "else"   -> ELSE
            "false"  -> FALSE
            "for"    -> FOR
            "fun"    -> FUN
            "if"     -> IF
            "nil"    -> NIL
            "or"     -> OR
            "print"  -> PRINT
            "return" -> RETURN
            "super"  -> SUPER
            "this"   -> THIS
            "true"   -> TRUE
            "var"    -> VAR
            "while"  -> WHILE
            _        -> IDENTIFIER
        tokenValue = if tokenType == TRUE then TRUE_LIT
                    else if tokenType == FALSE then FALSE_LIT
                    else if tokenType == NIL then NIL_LIT
                    else if tokenType == IDENTIFIER then ID ident
                    else NONE 
    in scanToken rest line (tokens ++ [TOKEN tokenType ident tokenValue line])

-- Handle numbers
scanNumber :: [Char] -> Int -> [Token] -> [Token]
scanNumber cs line tokens = 
    -- Extract integer part
    let (integerPart, rest) = span isDigit cs
        (fractionPart, remaining) = case rest of 
            --Check for decimal point
            ('.':d:ds) | isDigit d -> 
                --Extract fraction part
                let (frac, restt) = span isDigit (d:ds)
                -- Append the decimal point
                in ('.' : frac, restt)
            --no fraction part append empty string
            _ -> ("", rest)
        -- Add the integer and fraction parts
        number = integerPart ++ fractionPart
    in scanToken remaining line (tokens ++ [TOKEN NUMBER number (NUM (read number)) line])

-- Handle strings
string :: [Char] -> Int -> [Token] -> [Token]
string cs line tokens = scanString cs line "" tokens

-- Scan a string literal
scanString :: [Char] -> Int -> [Char] -> [Token] -> [Token]
-- Error if there is no closing quote
scanString [] line str token = error ("scanner error on line: " ++ show line ++ " - Unterminated string")
scanString ('\n':cs) line str tokens = scanString cs (line + 1) (str ++ "\n") tokens
-- After closing quote, we call scanToken to continue
scanString ('"':cs) line str tokens = scanToken cs line (tokens ++ [TOKEN STRING str (STR str) line])
scanString (c:cs) line str tokens = scanString cs line (str ++ [c]) tokens

isValidAlpha :: Char -> Bool
isValidAlpha c = isAlpha c || c == '_'

isValidNumericAlpha :: Char -> Bool
isValidNumericAlpha c = isValidAlpha c || isDigit(c)
