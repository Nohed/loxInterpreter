module Parser (parse) where

import AST
import Debug.Trace (trace)
import Scanner
import Tokens

{---------------------------------------------
 - Module : Parser
 -
 - @author :
 - 	c22nhd (c22nhd@cs.umu.se)
 -
 - @Version :
 - 	1.0 - 04/03-2025
 - 	        * Initial implementation
 -	2.0 - 09/03-2025
 -		* Variable declarations inside AST.hs missed a ";" at the end
 -		* Parsed incorrectly, changed order of parsing to correctly build tree
 -              * Refactorisations for readability
 ----------------------------------------------
 - @Description :
 -      Parser for the lox language,
 -      The modules main function is the Parse
 -        "parse :: [Token] -> SyntaxTree"
 -
 -      Helpfunction "parseFile" can be used to scan the parse a
 -      file and return a Syntaxtree
 -
 -
 -
 - @ON error :
 -      The function simply exits with a error message when a error occurs
 -      Error messages look somewhat like
 -        "foo ( } = parser error line 1 - "foo ( }" - invalid parenthesis"
 -
 -
 - @Further Improvment :
 -  TODO:
 -  Parse past errors - Right now the parser exits on the first error
 -                      Collecting all errors, and then exiting would be better
 -
 -  Language support :
 -      Currently no support for
 -            * FOR loops
 -            * Functions
 -      This should be implemented but not required for the assignment
 -}

-- Main parsing function
-- Parses a list of tokens into a syntax tree (List of declarations)
parse :: [Token] -> SyntaxTree
parse tokens =
  case parseDeclarations tokens of
    (declarations, []) -> SyntaxTree declarations
    (declarations, [TOKEN EOF _ _ _]) -> SyntaxTree declarations -- Accept EOF token at the end
    (_, remaining@(TOKEN t str line col : _)) ->
      error $ "Parser error line " ++ show line ++ " - unexpected " ++ show t ++ " token: " ++ str

-- Extra function helpful for testing the parser,
-- Scans then parses, thens prints a "lox" file
parseFile :: FilePath -> IO ()
parseFile filename = do
  file <- readFile filename -- Read the file
  let tokens = scanTokens file -- Scan the file
  let syntaxTree = parse tokens -- Parse the tokens
  print syntaxTree -- Print it

-- Parse declarations until EOF is reached
parseDeclarations :: [Token] -> ([Declaration], [Token])
-- This might cause issues when testing without a accual file? i think
parseDeclarations [] = sendError "Unexpected error, List of declarations is empty without EOF" [] -- This should never happen? EOF should always exist.
parseDeclarations (TOKEN EOF _ _ _ : _) = ([], []) -- EOF token reached, no more declarations.
parseDeclarations tokens =
  let (decl, rest) = parseDeclaration tokens -- get one declaration
      (decls, finalRest) = parseDeclarations rest -- Recursivly parse the rest of the declarations / Call "parseDeclaration"
   in (decl : decls, finalRest) -- Combine parsed dec with rest of decs

-- Parse one declaration
parseDeclaration :: [Token] -> (Declaration, [Token])
parseDeclaration (TOKEN VAR _ _ _ : rest) = parseVarDeclaration rest -- If the start is "var", parse as Variable
parseDeclaration tokens =
  let (stmt, rest) = parseStatement tokens -- Parse statement into a stmt(statement) and rest of tokens
   in (Statement stmt, rest)

-- Parse a variable declaration: "var name = expr" or "var name"
parseVarDeclaration :: [Token] -> (Declaration, [Token])
parseVarDeclaration tokens =
  case tokens of
    (TOKEN IDENTIFIER _ _ _ : TOKEN EQUAL _ _ _ : rest) ->
      -- match pattern var "IDENTIFIER =" like "var a = 5"
      let name = head tokens -- Get token with variable name
          (initializer, restTokens) = parseExpression rest -- Parse everything after the "=" like "= (9+5)" or just (= 5)
       in case restTokens of
            (TOKEN SEMICOLON _ _ _ : afterSemi) -> (VarDecl name (Just initializer), afterSemi)
            _ ->
              sendError "Expected a ';' after var declaration " restTokens -- No ";" found
    (TOKEN IDENTIFIER _ _ _ : TOKEN SEMICOLON _ _ _ : rest) ->
      -- No initalizer, just "var x;"
      let name = head tokens
       in (VarDecl name Nothing, rest)
    (TOKEN IDENTIFIER _ _ _ : _) ->
      sendError "Expected ';' after Var declaration" tokens -- "var" keyword and identifier found, but no following ";"
    _ ->
      sendError "Expected a variable name after 'Var' keyword" tokens

-- Parse a statement
-- NOTE: First 5 cases consumes the read token, and passes "rest"
-- " _ ->" does not. It passes entire "tokens"
parseStatement :: [Token] -> (Stmt, [Token])
parseStatement tokens =
  case tokens of
    (TOKEN PRINT _ _ _ : rest) -> parsePrint rest
    (TOKEN LEFT_BRACE _ _ _ : rest) -> parseBlock rest
    (TOKEN IF _ _ _ : rest) -> parseIF rest
    (TOKEN WHILE _ _ _ : rest) -> parseWhile rest
    _ -> parseExpressionStatement tokens

-- Parse BLOCK
-- Returns a Statement and the rest of the tokens
parseBlock :: [Token] -> (Stmt, [Token])
parseBlock tokens =
  let (statements, restTokens) = parseInsideBlock tokens -- Fetch everything inside block until ")" is found
   in case restTokens of
        (TOKEN RIGHT_BRACE _ _ _ : afterBrace) -> (Block statements, afterBrace) -- ")" found
        _ -> sendError "Expected a '}' to close Block" restTokens -- No closing brace found, ERROR

-- Parse statements within a block until we reach a '}'
parseInsideBlock :: [Token] -> ([Stmt], [Token])
parseInsideBlock tokens@(TOKEN RIGHT_BRACE _ _ _ : _) = ([], tokens) -- Right brace found, return it along with rest of tokens
parseInsideBlock tokens@(TOKEN EOF _ line _ : _) =
  error $ "Unterminated block, missing '}' at line " ++ show line -- Eof without right brace
parseInsideBlock tokens =
  let (stmt, afterStmt) = parseStatement tokens
      (stmts, afterStmts) = parseInsideBlock afterStmt
   in (stmt : stmts, afterStmts)

-- Parse IF
parseIF :: [Token] -> (Stmt, [Token])
parseIF tokens =
  case tokens of
    (TOKEN LEFT_PAREN _ _ _ : afterLeft) ->
      -- First toke is "("
      let (condition, afterCond) = parseExpression afterLeft -- Get the condition inside braces
       in case afterCond of
            (TOKEN RIGHT_PAREN _ _ _ : afterRight) ->
              -- Correct ")" closing brace found
              let (thenBranch, afterThen) = parseStatement afterRight -- Parse expression inside (Run on True)
               in case afterThen of
                    (TOKEN ELSE _ _ _ : afterElse) ->
                      -- Else found
                      -- Parse the Else statement (Run on False)
                      let (elseBranch, afterElse') = parseStatement afterElse -- Parse whats inside ELSE
                       in (IfStmt condition thenBranch (Just elseBranch), afterElse')
                    -- No else found, simply return the IF
                    _ -> (IfStmt condition thenBranch Nothing, afterThen)
            _ -> sendError "Expected a ')' after the IF condition" afterCond -- No closing brace found after condition
    _ -> sendError "Expected a '(' after the IF" tokens -- No left paren found, this is needed

-- Parse while
parseWhile :: [Token] -> (Stmt, [Token])
parseWhile tokens =
  case tokens of
    (TOKEN LEFT_PAREN _ _ _ : afterLeft) ->
      -- Left paren exist, no error
      let (condition, afterCond) = parseExpression afterLeft -- Get the condition inside while
       in case afterCond of
            (TOKEN RIGHT_PAREN _ _ _ : afterRight) ->
              -- ")" found
              let (content, afterContent) = parseStatement afterRight
               in (While condition content, afterContent)
            _ -> sendError "Expected ')' after '(' for the WHILE " afterCond -- ")" Is NOT found
    _ -> sendError "Expected '(' after WHILE" tokens

-- Parse Print stmt
parsePrint :: [Token] -> (Stmt, [Token])
parsePrint tokens =
  let (expr, restTokens) = parseExpression tokens -- Load expression to be printed
   in case restTokens of
        (TOKEN SEMICOLON _ _ _ : afterSemi) -> (Print expr, afterSemi) -- If ";" found, ret
        _ -> sendError "Expected a ';' after printed value" restTokens

-- Parse an expression statement
parseExpressionStatement :: [Token] -> (Stmt, [Token])
parseExpressionStatement tokens =
  let (expr, restTokens) = parseExpression tokens
   in case restTokens of
        (TOKEN SEMICOLON _ _ _ : afterSemi) -> (ExpressionStmt expr, afterSemi)
        _ -> sendError "Expected a ';' after expression" restTokens

-- Parse an expression
parseExpression :: [Token] -> (Expression, [Token])
parseExpression = parseAssignment

-- Parse assignment
parseAssignment :: [Token] -> (Expression, [Token])
parseAssignment tokens =
  let (expr, restTokens) = parseLogicOr tokens
   in case restTokens of
        (TOKEN EQUAL _ _ _ : afterEqual) ->
          let (value, afterValue) = parseAssignment afterEqual
           in case expr of
                Variable name -> (Assign name value, afterValue)
                _ -> sendError "Invalid assigment" afterValue
        _ -> (expr, restTokens)

parseTerm :: [Token] -> (Expression, [Token])
parseTerm tokens =
  let (left, rest) = parseFactor tokens
   in parseTermRest left rest

parseTermRest :: Expression -> [Token] -> (Expression, [Token])
parseTermRest left tokens =
  case tokens of
    (TOKEN t _ _ _ : rest)
      | t `elem` [PLUS, MINUS] ->
          let (right, afterRight) = parseFactor rest
              newLeft = Binary left (head tokens) right
           in parseTermRest newLeft afterRight
    _ -> (left, tokens)

parseFactor :: [Token] -> (Expression, [Token])
parseFactor tokens =
  let (left, rest) = parseUnary tokens
   in parseFactorRest left rest

parseFactorRest :: Expression -> [Token] -> (Expression, [Token])
parseFactorRest left tokens =
  case tokens of
    (TOKEN t _ _ _ : rest)
      | t `elem` [STAR, SLASH] ->
          let (right, afterRight) = parseUnary rest
              newLeft = Binary left (head tokens) right
           in parseFactorRest newLeft afterRight
    _ -> (left, tokens)

parseComparison :: [Token] -> (Expression, [Token])
parseComparison tokens =
  let (left, rest) = parseTerm tokens
   in parseComparisonRest left rest

parseComparisonRest :: Expression -> [Token] -> (Expression, [Token])
parseComparisonRest left tokens =
  case tokens of
    (TOKEN t _ _ _ : rest)
      | t `elem` [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] ->
          let (right, afterRight) = parseTerm rest
              newLeft = Binary left (head tokens) right
           in parseComparisonRest newLeft afterRight
    _ -> (left, tokens)

parseEquality :: [Token] -> (Expression, [Token])
parseEquality tokens =
  let (left, rest) = parseComparison tokens
   in parseEqualityRest left rest

parseEqualityRest :: Expression -> [Token] -> (Expression, [Token])
parseEqualityRest left tokens =
  case tokens of
    (TOKEN t _ _ _ : rest)
      | t `elem` [BANG_EQUAL, EQUAL_EQUAL] ->
          let (right, afterRight) = parseComparison rest
              newLeft = Binary left (head tokens) right
           in parseEqualityRest newLeft afterRight
    _ -> (left, tokens)

parseLogicAnd :: [Token] -> (Expression, [Token])
parseLogicAnd tokens =
  let (left, rest) = parseEquality tokens
   in parseLogicAndRest left rest

parseLogicAndRest :: Expression -> [Token] -> (Expression, [Token])
parseLogicAndRest left tokens =
  case tokens of
    (TOKEN AND _ _ _ : rest) ->
      let (right, afterRight) = parseEquality rest
          newLeft = Logic left (head tokens) right
       in parseLogicAndRest newLeft afterRight
    _ -> (left, tokens)

parseLogicOr :: [Token] -> (Expression, [Token])
parseLogicOr tokens =
  let (left, rest) = parseLogicAnd tokens
   in parseLogicOrRest left rest

parseLogicOrRest :: Expression -> [Token] -> (Expression, [Token])
parseLogicOrRest left tokens =
  case tokens of
    (TOKEN OR _ _ _ : rest) ->
      let (right, afterRight) = parseLogicAnd rest
          newLeft = Logic left (head tokens) right
       in parseLogicOrRest newLeft afterRight
    _ -> (left, tokens)

parseUnary :: [Token] -> (Expression, [Token])
parseUnary tokens@(TOKEN t _ _ _ : rest)
  | t `elem` [BANG, MINUS] =
      let (right, afterRight) = parseUnary rest
       in (Unary (head tokens) right, afterRight)
parseUnary tokens = parsePrimary tokens

-- Parse primary
parsePrimary :: [Token] -> (Expression, [Token])
parsePrimary [] = sendError "Expression ended incorrectly " []
parsePrimary tokens =
  case tokens of
    (TOKEN TRUE _ _ _ : rest) -> (Literal TRUE_LIT, rest)
    (TOKEN FALSE _ _ _ : rest) -> (Literal FALSE_LIT, rest)
    (TOKEN NIL _ _ _ : rest) -> (Literal NIL_LIT, rest)
    (TOKEN NUMBER _ numb _ : rest) -> (Literal numb, rest)
    (TOKEN STRING _ str _ : rest) -> (Literal str, rest)
    (TOKEN IDENTIFIER _ _ _ : rest) -> (Variable (head tokens), rest)
    (TOKEN LEFT_PAREN _ _ _ : rest) ->
      -- Parse whats inside "()" like (x+y)
      let (expr, restTokens) = parseExpression rest -- Get expression
       in case restTokens of
            -- Get ")"
            (TOKEN RIGHT_PAREN _ _ _ : afterParen) -> (Grouping expr, afterParen)
            _ ->
              sendError "Expected ')' after expression" restTokens -- Grouping without a ")", throw error
    (TOKEN EOF _ line _ : _) ->
      -- Unexpected EOF
      sendError "Unexpected EOF found" tokens
    (TOKEN t str _ line : _) ->
      -- Unnexpected symbol
      error $ "Parser error line " ++ show line ++ " - unexpected token" ++ show t ++ str

sendError :: String -> [Token] -> a
sendError msg [] = error $ "Parser error at end of file - " ++ msg
sendError msg (TOKEN _ _ _ line : _) =
  error $ "Parser error line " ++ show line ++ " - " ++ msg
