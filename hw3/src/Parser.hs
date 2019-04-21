module Parser
  ( codeParse
  ) where

import Control.Applicative ((<|>))
import Data.Void (Void)
import GrammarExpressions (AssignmentData (..), Code (..), ValueBuilder (..), VarName (..),
                           VarValue (..))
import Text.Megaparsec (Parsec, anySingle, eof, many, manyTill, parse)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, eol, space)

type Parser = Parsec Void String

parseInt :: Parser String
parseInt = do
  ch <- fmap Left digitChar <|> fmap Right (pure ())
  case ch of
    Left dgt -> do
      suffix <- parseInt
      return $ dgt : suffix
    Right _ -> return []

varParser :: Parser String
varParser = do
  first <- fmap Left digitChar <|> fmap Right (many $ alphaNumChar <|> char '_')
  case first of
    Left dgt -> do
      suffix <- parseInt
      return $ dgt : suffix
    Right ans -> return ans

data Triple a b c
  = First a
  | Second b
  | Third c

parseValue :: Parser [ValueBuilder]
parseValue = do
  chr <- anySingle
  case chr of
    '\\' -> do
      nextChr <- char '$' <|> char '\\'
      return [ValueBuilderElement [nextChr]]
    '\'' -> parseSingleQuotes'
    '$' -> parseVarWithDollar'
    ch -> return [ValueBuilderElement [ch]]

appendToValueBuilderList :: ValueBuilder -> [ValueBuilder] -> [ValueBuilder]
appendToValueBuilderList el [] = [el]
appendToValueBuilderList el@(Argument _) l = el : l
appendToValueBuilderList el@(ValueBuilderElement _) l@(Argument _:_) = el : l
appendToValueBuilderList (ValueBuilderElement i) (ValueBuilderElement j:xs) = (ValueBuilderElement $ i ++ j) : xs

parseVarWithDollar' :: Parser [ValueBuilder]
parseVarWithDollar' = do
  value <- varParser
  return [Argument value]

parseSingleQuotes' :: Parser [ValueBuilder]
parseSingleQuotes' = do
  value <- manyTill anySingle (char '\'')
  return [ValueBuilderElement value]

parseAssignmentValue :: Parser [ValueBuilder]
parseAssignmentValue = do
  value <- parseValue
  continue <-
    fmap Left (many (char ' ') *> fmap First (char ';') <|> fmap Second eof <|> fmap Third eol) <|> fmap Right (pure ())
  case continue of
    Left _ -> return value
    Right _ -> do
      suffix <- parseAssignmentValue
      case value of
        [ValueBuilderElement _] -> return $ appendToValueBuilderList (head value) suffix
        _                       -> return $ value ++ suffix

assignment :: Parser AssignmentData
assignment = do
  _ <- space
  var <- varParser
  _ <- char '='
  AssignmentData (VarName var) . VarValue <$> parseAssignmentValue

code :: Parser [AssignmentData]
code = do
  _ <- many eol
  line <- fmap Left assignment <|> fmap Right eof
  case line of
    Left assign -> do
      next <- code
      return $ assign : next
    Right _ -> return []

codeParse :: String -> IO Code
codeParse input =
  case parse code "log.txt" input of
    Left err -> do
      print err
      return (Code [])
    Right parsedCode -> return $ Code parsedCode
