{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes   #-}

module Task3 where

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
      return $ (resultF resultA, tailA)

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
  empty = Parser $ \_ -> Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser p1) (Parser p2) = Parser $ \s -> p1 s <|> p2 s
