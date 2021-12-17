module ParseUtils 
  ( Parser
  , digit
  , digitLine
  , eolv
  , int
  , ints
  , integer
  , intline
  , letter
  , lexeme
  , parens
  , parseFile
  , parseFromFile
  , parseGrid
  , parseMap
  , parseString
  , strline
  , symbol
  ) where

import Data.Char (isAlpha, isDigit, ord)
import Data.Default (Default, def)
import Data.Functor ((<&>))
import Data.Void ( Void )
import qualified Data.Map as M
import Text.Megaparsec
    ( (<|>),
      Parsec,
      ParseErrorBundle,
      (<?>),
      getSourcePos,
      oneOf,
      optional,
      parse,
      satisfy,
      unPos,
      between,
      many,
      runParser,
      some,
      MonadParsec(hidden, eof),
      SourcePos(sourceLine, sourceColumn) )
import Text.Megaparsec.Char ( digitChar, eol, space, string, letterChar )

type Parser = Parsec Void String

parseFile :: Default a => Parser a -> FilePath -> IO a
parseFile p f = do
  xs <- readFile f 
  case parse p f xs of
    (Left e)   -> print e >> pure def
    (Right ys) -> pure ys

parseFromFile :: Parsec e String a -> String -> IO (Either (ParseErrorBundle String e) a)
parseFromFile p file = runParser p file <$> readFile file

parseString :: Default a => Parser a -> String -> a
parseString p xs = 
  case parse p "" xs of
    (Left _)   -> def
    (Right ys) -> ys

parseMap :: Parser a -> Parser (M.Map (Int, Int) a)
parseMap p = some (lineParser p) <&> M.fromList . concat

lineParser :: Parser a -> Parser [((Int, Int), a)]
lineParser p = some (locnParser p) >>= \xs -> eolv <|> eof >> pure xs

locnParser :: Parser a -> Parser ((Int, Int), a)
locnParser p = do
  c <- p
  pos <- getSourcePos
  pure ((unPos (sourceLine pos), unPos (sourceColumn pos)), c)

intline :: Parser Int
intline = do
  x <- int                         -- consume the digits
  _ <- many (oneOf [' ', '\t'])    -- consume any trailing spaces
  _ <- eolv <|> eof                -- consume the end-of-line character, or end-of-file
  pure x                           -- return the number we want

digitLine :: Parser [Int]
digitLine = do
  xs <- some digitChar
  _ <- eolv <|> eof
  pure $ map (\x -> read [x] :: Int) xs

strline :: Parser String
strline = do
  s <- some letterChar
  _ <- eolv <|> eof                -- consume the end-of-line character, or end-of-file
  pure s

eolv :: Parser ()
eolv = ignore eol

digit :: Parser Char
digit = satisfy isDigit <?> "digit"

int :: Parser Int
int = do
  sign <- optional (symbol "-")
  xs <- some digit
  let x = read xs :: Int
  pure $ case sign of
          Nothing -> x
          Just _  -> -1 * x

ints :: String -> Parser [Int]
ints delim = many $ do
  x <- int
  ignore $ optional (string delim)
  pure x

ignore :: Parser a -> Parser ()
ignore p = p >> pure ()

lexeme :: Parser a -> Parser a
lexeme p = p <* hidden space

integer :: Parser Integer
integer = lexeme (read <$> some digitChar <?> "integer")

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

letter :: Parser Char
letter = satisfy isAlpha <?> "letter"

parseGrid :: Parser [[Int]]
parseGrid = do
  m <- some rowP
  _ <- optional eof 
  pure m

rowP :: Parser [Int]
rowP = do
  xs <- some digit
  _ <- optional eol
  pure $ map (\x -> ord x - 48) xs