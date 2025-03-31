module ParseTypes where
import Tokens
import Data.List (intercalate) 

-- Declarations
data Declaration
    = FunDecl Token (Maybe[Token]) Stmt
    | VarDecl Token (Maybe Expr) 
    | Statement Stmt

instance Show Declaration where
    show (FunDecl name (Just params) body) = "F DEC -> " ++ showToken name ++ "(" ++ paramsToString params ++ ")" ++ show body
    show (FunDecl name Nothing body) = "F DEC -> " ++ showToken name ++ "()" ++ show body
    show (VarDecl token Nothing) = "V DEC -> " ++ showToken token ++ ";"
    show (VarDecl token (Just expr)) = "V DEC -> " ++ showToken token ++ "=" ++ show expr ++ ";"
    show (Statement stmt) = show stmt

paramsToString :: [Token] -> String
paramsToString [] = ""
paramsToString [p] = showToken p
paramsToString (p:ps) = showToken p ++ "," ++ paramsToString ps
--Expression 
data Expr 
    = Assign Token Expr
    | Binary Expr Token Expr 
    | Call Expr Token [Expr] 
    | Grouping Expr 
    | Literal Literal
    | Logical Expr Token Expr 
    | Unary Token Expr 
    | Variable Token 

instance Show Expr where
    show (Literal l) = show l
    show (Binary l op r) = "("++ show l ++ showLogicalOp op  ++ show r ++ ")"
    show (Unary op expr) = "(" ++ showLogicalOp op ++ show expr ++ ")"
    show (Grouping expr) = "("++ show expr ++ ")"
    show (Variable var) = showToken var
    show (Call expr _ args) = show expr  ++"(" ++ intercalate "," (map show args)  ++ ")"
    show (Assign var expr) = showToken var ++ "=" ++ show expr
    

-- Statement 
data Stmt
    = Block [Declaration] 
    | Expression Expr
    | If Expr Stmt (Maybe Stmt) 
    | Print Expr 
    | Return (Maybe Expr) 
    | While Expr Stmt
    | For (Maybe Declaration) (Maybe Expr) (Maybe Expr) Stmt
    
instance Show Stmt where
    show (Expression expr) = show expr ++ ";"
    show (Print expr) = "print " ++ show expr ++ ";"
    show (Block declarations) = "{" ++ unwords(map show declarations) ++ "}"
    show (Return Nothing) = "return;"
    show (Return (Just expr)) = "return " ++ show expr ++ ";"
    show (If expr stmt Nothing) = "if(" ++ show expr ++ ") " ++ show stmt
    show (If expr stmt (Just elseStmt)) = "if(" ++ show expr ++ ") " ++ show stmt ++ "else" ++ show elseStmt
    show (While cond body) = "while(" ++ show cond ++ ") " ++ show body
    show (For Nothing Nothing Nothing stmt) = "for(;;)" ++ show stmt
    show (For Nothing Nothing (Just incr) stmt) = "for(;;" ++ show incr ++ ")" ++ show stmt
    show (For Nothing (Just cond) Nothing stmt) = "for(;" ++ show cond ++ ";)" ++ show stmt
    show (For Nothing (Just cond) (Just incr) stmt) = "for(;" ++ show cond ++ ";" ++ show incr ++ ")" ++ show stmt
    show (For (Just init) Nothing Nothing stmt) = "for(" ++ show init ++ " ;)" ++ show stmt
    show (For (Just init) Nothing (Just incr) stmt) = "for(" ++ show init ++ " ;" ++ show incr ++ ")" ++ show stmt
    show (For (Just init) (Just cond) Nothing stmt) = "for(" ++ show init ++ " " ++ show cond ++ ";)" ++ show stmt
    show (For (Just init) (Just cond) (Just incr) stmt) = "for(" ++ show init  ++ " " ++ show cond ++ "; " ++ show incr ++ ")" ++ show stmt
    
data ParseTree
    = ParseTree [Declaration]

instance Show ParseTree where
    show (ParseTree decls) = (show $ length decls) ++ "\n" ++ (unlines $ map show decls)



-- Helper functions
showLiteral :: Literal -> String
showLiteral (STR s) = "\"" ++ s ++ "\""
showLiteral (NUM n) = show n
showLiteral TRUE_LIT = "TRUE_LIT"
showLiteral FALSE_LIT = "FALSE_LIT"
showLiteral NIL_LIT = "NIL_LIT"

showToken :: Token -> String
showToken (TOKEN _ s _ _) = s

showLogicalOp :: Token -> String
showLogicalOp (TOKEN AND _ _ _) = "&&"
showLogicalOp (TOKEN OR _ _ _) = "||"
showLogicalOp token = showToken token