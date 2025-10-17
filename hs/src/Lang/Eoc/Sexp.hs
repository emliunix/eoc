module Lang.Eoc.Sexp where

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Functor (($>))

-- | S-expression data type
data Sexp
  = Symbol String
  | Integer Integer
  | Bool Bool
  | Nil
  | Colon
  | String String
  | List [Sexp]
  deriving (Show, Eq)

-- | Parse a symbol (identifier)
-- Symbols start with a letter or special character and can contain letters, digits, and special chars
parseSymbol :: Parser Sexp
parseSymbol = do
  first <- try letter <|> oneOf "+-*/<>=!?$%&@^_~"
  rest <- many (try alphaNum <|> oneOf "+-*/<>=!?$%&@^_~")
  let sym = first : rest
  return $ Symbol sym

-- | Parse an integer (positive or negative)
parseInteger :: Parser Sexp
parseInteger = do
  sign <- optional (char '-')
  digits <- some digit
  let num = read digits
  return $ Integer $ case sign of
    Just _ -> -num
    Nothing -> num

-- | Parse a boolean (#t or #f)
parseBool :: Parser Sexp
parseBool = do
  _ <- char '#'
  value <- try (char 't') <|> char 'f'
  return $ Bool (value == 't')

-- | Parse nil ('())
parseNil :: Parser Sexp
parseNil = do
  _ <- string "'()"
  return Nil

-- | Parse a colon (:)
parseColon :: Parser Sexp
parseColon = do
  _ <- char ':'
  return Colon

-- | Parse a string literal
parseStringLiteral :: Parser Sexp
parseStringLiteral = do
  _ <- char '"'
  content <- many stringChar
  _ <- char '"'
  return $ String content
  where
    stringChar = try (noneOf "\"") <|> (char '\\' >> anyChar)

-- | Parse whitespace and comments
whitespace :: Parser ()
whitespace = skipMany (try whitespaceChar <|> comment)
  where
    whitespaceChar = do
      _ <- oneOf " \t\n\r"
      return ()
    comment = do
      _ <- char ';'
      _ <- many (noneOf "\n\r")
      _ <- optional (oneOf "\n\r")
      return ()

-- | Parse an atom (symbol, integer, bool, nil, or string)
parseAtom :: Parser Sexp
parseAtom = try parseNil
  <|> try parseColon
  <|> try parseBool
  <|> try parseInteger
  <|> try parseStringLiteral
  <|> parseSymbol

-- | Parse a list of s-expressions
parseList :: Parser Sexp
parseList = do
  paren <- try (char '(' $> ')') <|> (char '[' $> ']')
  whitespace
  elements <- many (parseSexp <* whitespace)
  _ <- char paren
  return $ List elements

-- | Parse any s-expression
parseSexp :: Parser Sexp
parseSexp = do
  whitespace
  result <- try parseAtom <|> parseList
  whitespace
  return result

-- | Parse a complete s-expression from a string
parseSexpFromString :: String -> Result Sexp
parseSexpFromString = parseString parseSexp mempty

-- | Parse multiple s-expressions from a string
parseSexpsFromString :: String -> Result [Sexp]
parseSexpsFromString = parseString (many parseSexp <* eof) mempty
