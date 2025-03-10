module AST where

import Tokens

data SyntaxTree = SyntaxTree [Declaration]

instance Show SyntaxTree where
  show (SyntaxTree decs) = (show $ length decs) ++ "\n" ++ (unlines $ map show decs)

data Declaration
  = VarDecl Token (Maybe Expression)
  | Statement Stmt

instance Show Declaration where
  show (VarDecl token Nothing) = "V DEC -> " ++ tokenToString token ++ ";"
  show (VarDecl token (Just expr)) = "V DEC -> " ++ tokenToString token ++ "=" ++ show expr ++ ";"
  show (Statement stmt) = show stmt

data Expression
  = Literal Literal -- Numbers, Strings, Booleans
  | Variable Token -- Variable names
  | Unary Token Expression -- -5, !true
  | Binary Expression Token Expression -- a + b, a * b
  | Assign Token Expression -- a = b
  | Grouping Expression -- (a + b)
  | Logic Expression Token Expression -- a and b, a or b

instance Show Expression where
  show (Literal lit) = litToString lit
  show (Variable v) = tokenToString v
  show (Unary u expr) = "(" ++ tokenToString u ++ show expr ++ ")"
  show (Binary left b right) = "(" ++ show left ++ tokenToString b ++ show right ++ ")"
  show (Assign var valueToAssign) = tokenToString var ++ "=" ++ show valueToAssign
  show (Grouping expr) = "(" ++ show expr ++ ")"
  show (Logic left l right) = "(" ++ show left ++ logicToString l ++ show right ++ ")"

data Stmt
  = ExpressionStmt Expression -- Just an expression (e.g., `5+3;`)
  | Print Expression -- print a value
  | Block [Stmt] -- { stmt1; stmt2; }
  | IfStmt Expression Stmt (Maybe Stmt) -- if (expr) {stmt} else {stmt}
  | While Expression Stmt -- while (expr) {stmt}

instance Show Stmt where
  show (ExpressionStmt expr) = show expr ++ ";"
  show (Print expr) = "print" ++ show expr ++ ";"
  show (Block stmts) = "{" ++ concatMap show stmts ++ "}"
  show (IfStmt cond thenStmt Nothing) = "if(" ++ show cond ++ ")" ++ show thenStmt
  show (IfStmt cond thenStmt (Just elseStmt)) = "if(" ++ show cond ++ ")" ++ show thenStmt ++ "else" ++ show elseStmt
  show (While cond body) = "while(" ++ show cond ++ ")" ++ show body

-- Convert token values to more printable strings --

-- Token
tokenToString :: Token -> String
tokenToString (TOKEN _ str _ _) = str -- Get just the string that created the token

-- Literals
litToString :: Literal -> String
litToString (STR s) = "\"" ++ s ++ "\"" -- Print quotes around string
litToString (NUM fl) = show fl -- Use floats std show
litToString TRUE_LIT = "TRUE_LIT"
litToString FALSE_LIT = "FALSE_LIT"
litToString NIL_LIT = "NIL_LIT"

logicToString :: Token -> String
logicToString (TOKEN AND _ _ _) = "&&"
logicToString (TOKEN OR _ _ _) = "||"
-- \|| for OR
logicToString token = tokenToString token -- Fallback
