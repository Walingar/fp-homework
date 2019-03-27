{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes   #-}

module Task3
  ( Parser(..)
  , ok
  , eof
  , satisfy
  , element
  , stream
  , isCBS
  , parseInt
  , parseIntList
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)

newtype Parser s a = Parser
  { runParser :: [s] -> Maybe (a, [s])
  }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser f) (Parser parser) =
    Parser $ \s -> do
      (resultF, tailF) <- f s
      (resultA, tailA) <- parser tailF
      return (resultF resultA, tailA)

instance Monad (Parser s) where
  (>>=) :: forall a b. Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser parser) f =
    Parser $ \s ->
      case parser s of
        Nothing -> Nothing
        Just (resultA, tailA) ->
          let (Parser parserF) = f resultA
           in parserF tailA

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser p1) (Parser p2) = Parser $ \s -> p1 s <|> p2 s

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof =
  Parser $ \s ->
    case s of
      [] -> Just ((), s)
      _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \s ->
    case s of
      [] -> Nothing
      (x:xs) ->
        if p x
          then Just (x, xs)
          else Nothing

element :: Eq s => s -> Parser s s
element c = satisfy (== c)

stream :: Eq s => [s] -> Parser s [s]
stream [] = Parser $ \s -> Just ([], s)
stream (x:xs) = do
  _ <- element x
  stream xs

anyElement :: Parser s s
anyElement = satisfy (const True)

isCBS :: String -> Bool
isCBS inp =
  case runParser (pspParserKek 0) inp of
    Nothing -> False
    _       -> True

pspParserKek :: Int -> Parser Char (Either Char ())
pspParserKek balance =
  if balance == 0
    then do
      parsed <- fmap Left (element '(') <|> fmap Right eof
      case parsed of
        Left _  -> pspParserKek (balance + 1)
        Right _ -> pure parsed
    else do
      parsed <- anyElement
      if parsed == '('
        then pspParserKek (balance + 1)
        else pspParserKek (balance - 1)

parseInt :: String -> Maybe Int
parseInt inp =
  case runParser intSignParser inp of
    Nothing         -> Nothing
    Just (value, _) -> Just $ signedIntToInt value

data SignedInt =
  SignedInt Char
            Int

signedIntToInt :: SignedInt -> Int
signedIntToInt (SignedInt '+' value) = value
signedIntToInt (SignedInt '-' value) = -value
signedIntToInt _                     = error "Unexpected sign"

parseSign :: Parser Char (Either Char ())
parseSign = fmap Left (satisfy (`elem` ['+', '-'])) <|> fmap Right ok

parseDigit :: Parser Char Int
parseDigit = charToInt <$> satisfy (`elem` ['0' .. '9'])

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt _   = error "Unexpected Char"

nextInt :: Parser Char Int
nextInt = do
  parsedSignIntPart <- parseSign
  parsedInt <- nextIntParser
  case parsedSignIntPart of
    Left ch -> return $ signedIntToInt $ SignedInt ch parsedInt
    Right _ -> return $ signedIntToInt $ SignedInt '+' parsedInt

nextIntParser :: Parser Char Int
nextIntParser = do
  firstDigit <- parseDigit
  (value, len) <- parseNextIntTail
  return $ firstDigit * (10 ^ len) + value

eofNextInt :: Parser Char ()
eofNextInt = do
  skip [' ']
  _ <- fmap Left (satisfy (== ',')) <|> fmap Right eof
  ok

parseNextIntTail :: Parser Char (Int, Int)
parseNextIntTail = do
  parsed <- fmap Left parseDigit <|> fmap Right eofNextInt
  case parsed of
    Left digit -> parseNextIntTail' digit
    Right _    -> return (0, 0)

parseNextIntTail' :: Int -> Parser Char (Int, Int)
parseNextIntTail' digit = do
  (value, len) <- parseNextIntTail
  return (digit * (10 ^ len) + value, len + 1)

intSignParser :: Parser Char SignedInt
intSignParser = do
  parsedSignIntPart <- parseSign
  parsedInt <- intParser
  case parsedSignIntPart of
    Left ch -> return $ SignedInt ch parsedInt
    Right _ -> return $ SignedInt '+' parsedInt

intParser :: Parser Char Int
intParser = do
  firstDigit <- parseDigit
  (value, len) <- parseIntTail
  return $ firstDigit * (10 ^ len) + value

parseIntTail :: Parser Char (Int, Int)
parseIntTail = do
  parsed <- fmap Left parseDigit <|> fmap Right eof
  case parsed of
    Left digit -> parseIntTail' digit
    Right _    -> return (0, 0)

parseIntTail' :: Int -> Parser Char (Int, Int)
parseIntTail' digit = do
  (value, len) <- parseIntTail
  return (digit * (10 ^ len) + value, len + 1)

skip :: Eq a => [a] -> Parser a ()
skip list = do
  parsed <- fmap Left (satisfy (`elem` list)) <|> fmap Right ok
  case parsed of
    Left _  -> skip list
    Right _ -> return ()

parseIntList :: String -> [[Int]]
parseIntList inp =
  case runParser intListParser inp of
    Nothing         -> error "WHOOOOPS"
    Just (value, _) -> value

intListParser :: Parser Char [[Int]]
intListParser = do
  parsed <- fmap Left intSingleListParser <|> fmap Right eof
  case parsed of
    Left list -> parseIntSingleListTail list
    Right _   -> return []

parseIntSingleListTail :: [Int] -> Parser Char [[Int]]
parseIntSingleListTail list = do
  parsed <- intListParser
  return $ list : parsed

intSingleListParser :: Parser Char [Int]
intSingleListParser = do
  skip [' ']
  count <- nextInt
  skip [' ']
  parseNInt count

parseNInt :: Int -> Parser Char [Int]
parseNInt 0 = return []
parseNInt count = do
  skip [' ']
  number <- nextInt
  skip [' ']
  numbers <- parseNInt $ count - 1
  return $ number : numbers
