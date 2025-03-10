module Scanner (scanTokens) where

-- Dependencies
import Data.Char
import Data.Maybe
import System.IO
import Tokens -- As defined in course gitlab

{---------------------------------------------
 - Module : Scanner
 -
 - @author :
 - 	c22nhd (c22nhd@cs.umu.se)
 -
 - @Version :
 - 	1.0 - 11/02-2025
 -
 ----------------------------------------------
 - @Description
 -
 -        A scanner (Lexer) for the LOX language defined in "Crafting interpreters"
 -        The module takes a List of characters and returns a list of "TOKENS" defined in tokens.hs
 -
 -        The scanner reads characters recursively, character by character and creates tokens
 -
 -}
scanTokens :: [Char] -> [Token]
scanTokens str = reverse (scan str [] 1) -- For complexity´s sake, tokens are prepended to the list
-- To facilitate this, the list is reversed to ensure correct order

-- <Scan>
-- Takes:
-- 	[Char] tokens  - Input
-- 	[Token] - List of created tokens/ Tokens to be created
-- 	Integer - Row number
-- Gives:
-- 	List of tokens
scan :: [Char] -> [Token] -> Int -> [Token]
scan [] tokens row = TOKEN EOF "" NONE row : tokens -- Base: If [Char] is empty, add EOF token to tokens

-- Recursive case
scan (c : cs) tokens row
  | isSingleChar c = scan cs (TOKEN (charToType c) [c] NONE row : tokens) row
  | c == '!' = match BANG_EQUAL BANG cs tokens row
  | c == '=' = match EQUAL_EQUAL EQUAL cs tokens row
  | c == '<' = match LESS_EQUAL LESS cs tokens row
  | c == '>' = match GREATER_EQUAL GREATER cs tokens row
  | c == '/' =
      if not (null cs) && head cs == '/'
        then scanComment cs tokens row
        else scan cs (TOKEN (charToType c) [c] NONE row : tokens) row
  | c == '"' = scanString cs tokens row
  | c == '\n' = scan cs tokens (row + 1)
  | isDigit c = scanDigit (c : cs) tokens row
  | isAlpha c || c == '_' = scanIdentifier (c : cs) tokens row
  | isSpace c = scan cs tokens row -- Ignore blank space
  | otherwise = error $ "scanner error line " ++ show row ++ " - '" ++ [c] ++ "' is not a valid TokenType"
  where
    -- Boolean function, determines if char c is of type "(){},;.*+-" "Single char"
    isSingleChar :: Char -> Bool
    isSingleChar c = c `elem` "(){},;.*+-"

    -- Takes single char and returns TokenType
    charToType :: Char -> TokenType
    charToType c = case c of
      '(' -> LEFT_PAREN
      ')' -> RIGHT_PAREN
      '{' -> LEFT_BRACE
      '}' -> RIGHT_BRACE
      ',' -> COMMA
      '.' -> DOT
      '-' -> MINUS
      '+' -> PLUS
      ';' -> SEMICOLON
      '*' -> STAR
      '/' -> SLASH
      _ -> error "Unexpected single character token"

    -- Takes:
    --    matchToken - This is the character i have
    --    [Char] - Rest of input
    --    [Token] - Created Tokens
    match :: TokenType -> TokenType -> [Char] -> [Token] -> Int -> [Token]
    match matchToken haveToken (h : t) tokens row
      | not (null t) && h == '=' = scan t (TOKEN matchToken [c, h] NONE row : tokens) row -- Match found
      | otherwise = scan (h : t) (TOKEN haveToken [c] NONE row : tokens) row -- Match not found
    match _ haveToken [] tokens row = TOKEN haveToken [c] NONE row : tokens -- If the last character is a "=" [char] will be empty

{-
 - <scanComment>
 - Takes:
 -      [Char] input - Characters remaining to be read
 -      [Token] tokens - List of generated tokens
 -      Int row - Current row
 -
 - Gives:
 -      [Tokens] - List of generates Tokens
 -
 -
 - Uses dropWhile to remove all characters from "//" to the newline.
 - I.e dropping the whole comment
 -}
scanComment :: [Char] -> [Token] -> Int -> [Token]
scanComment input tokens row =
  let rest = dropWhile (/= '\n') input -- Drop all characters up to newline
   in case rest of
        [] -> scan [] tokens row -- End of input
        (_ : cs) -> scan cs tokens (row + 1) -- Skip newline and continue scanning

{-
 - <scanString>
 - Takes:
 -      [Char] input - Characters remaining to be read
 -      [Token] tokens - List of generated tokens
 -      Int row - Current row
 -
 - Gives:
 -      [Tokens] - List of generates Tokens
 -
 - Uses span function to get all characters from the current " to the next " and stores them inside "content"
 - Function throws a error on missing "
 -}
scanString :: [Char] -> [Token] -> Int -> [Token]
scanString chars tokens row =
  let (content, rest) = span (/= '"') chars
   in case rest of
        [] -> error $ "scanner error line " ++ show row ++ " No closing quote found" -- No closing quote
        (_ : cs) -> scan cs (TOKEN STRING content (STR content) row : tokens) row -- Closing qoute found, scan after it

{-
 - <scanDigit>
 - Takes:
 -      [Char] input - Characters remaining to be read
 -      [Token] tokens - List of generated tokens
 -      Int row - Current row
 -
 - Gives:
 -      [Tokens] - List of generates Tokens
 -

 -}
scanDigit :: [Char] -> [Token] -> Int -> [Token]
scanDigit chars tokens row =
  let (intContent, rest) = span isDigit chars -- Extract the integer part
   in case rest of
        ('.' : cs)
          | not (null cs) && isDigit (head cs) -> -- Handle the decimal point
              let (fracContent, rest') = span isDigit cs -- Extract the fractional part
                  numberString = intContent ++ "." ++ fracContent -- Combine integer and fractional parts as a string
                  numValue = read numberString :: Float -- Convert the string to a Float for NUM token
               in scan rest' (TOKEN NUMBER numberString (NUM numValue) row : tokens) row -- Add the token
        _ -> scan rest (TOKEN NUMBER intContent (NUM (read intContent :: Float)) row : tokens) row -- Handle integer numbers

{-
 - <scanIdentifier>
 - Takes:
 -      [Char] input - Characters remaining to be read
 -      [Token] tokens - List of generated tokens
 -      Int row - Current row
 -
 - Gives:
 -      [Tokens] - List of generates Tokens
 -
 -
 -}
scanIdentifier :: [Char] -> [Token] -> Int -> [Token]
scanIdentifier chars tokens row =
  let (identifier, rest) = span isIdentifierChar chars -- get the identifier
   in case identifier of
        "and" -> scan rest (TOKEN AND identifier NONE row : tokens) row
        "class" -> scan rest (TOKEN CLASS identifier NONE row : tokens) row
        "else" -> scan rest (TOKEN ELSE identifier NONE row : tokens) row
        "false" -> scan rest (TOKEN FALSE identifier FALSE_LIT row : tokens) row
        "for" -> scan rest (TOKEN FOR identifier NONE row : tokens) row
        "fun" -> scan rest (TOKEN FUN identifier NONE row : tokens) row
        "if" -> scan rest (TOKEN IF identifier NONE row : tokens) row
        "nil" -> scan rest (TOKEN NIL identifier NIL_LIT row : tokens) row
        "or" -> scan rest (TOKEN OR identifier NONE row : tokens) row
        "print" -> scan rest (TOKEN PRINT identifier NONE row : tokens) row
        "return" -> scan rest (TOKEN RETURN identifier NONE row : tokens) row
        "super" -> scan rest (TOKEN SUPER identifier NONE row : tokens) row
        "this" -> scan rest (TOKEN THIS identifier NONE row : tokens) row
        "true" -> scan rest (TOKEN TRUE identifier TRUE_LIT row : tokens) row
        "var" -> scan rest (TOKEN VAR identifier NONE row : tokens) row
        "while" -> scan rest (TOKEN WHILE identifier NONE row : tokens) row
        _ -> scan rest (TOKEN IDENTIFIER identifier (ID identifier) row : tokens) row -- If not a keyword, it's an identifier

-- Helper function
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'
