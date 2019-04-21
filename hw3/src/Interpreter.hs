module Interpreter
  ( interpret
  ) where

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map, fromList, insert, lookup)
import GrammarExpressions (AssignmentData (..), Code (..), ValueBuilder (..), VarName (..),
                           VarValue (..))

data Ctx =
  Ctx (String -> IO ())
      (IORef (Map String String))

logsF :: String -> IO ()
logsF toLog = putStrLn $ "[LOG] " ++ toLog

withIndex :: [String] -> [(String, String)]
withIndex arr = withIndex' arr 0
  where
    withIndex' :: [String] -> Int -> [(String, String)]
    withIndex' [] _       = []
    withIndex' (x:xs) ind = (show ind, x) : withIndex' xs (ind + 1)

interpret :: [String] -> Code -> IO ()
interpret args code = do
  argsMap <- newIORef $ fromList $ withIndex args
  runReaderT (interpret' code) (Ctx logsF argsMap)

interpret' :: Code -> ReaderT Ctx IO ()
interpret' (Code []) = do
  Ctx logs _ <- ask
  lift $ logs "Finished."
interpret' (Code (AssignmentData (VarName variable) (VarValue value):xs)) = do
  Ctx logs args <- ask
  varValue <- getVarValue variable value
  case varValue of
    Nothing -> return ()
    Just varValue' -> do
      lift $ logs $ "Variable: '" ++ variable ++ "'. Content: '" ++ varValue' ++ "'"
      argsMap <- lift $ readIORef args
      lift $ writeIORef args (insert variable varValue' argsMap)
      interpret' (Code xs)

-- TODO: use Maybe as monad here
getVarValue :: String -> [ValueBuilder] -> ReaderT Ctx IO (Maybe String)
getVarValue _ [] = return $ Just ""
getVarValue variable (ValueBuilderElement str:xs) = do
  suffix <- getVarValue variable xs
  case suffix of
    Nothing    -> return Nothing
    Just value -> return $ Just $ str ++ value
getVarValue variable (Argument name:xs) = do
  Ctx logs vars <- ask
  argsMap <- lift $ readIORef vars
  case Data.Map.lookup name argsMap of
    Just value -> do
      suffix <- getVarValue variable xs
      case suffix of
        Nothing     -> return Nothing
        Just value' -> return $ Just $ value ++ value'
    Nothing -> do
      lift $ logs $ "Variable '" ++ variable ++ "'. Couldn't find argument '" ++ name ++ "'"
      return Nothing
