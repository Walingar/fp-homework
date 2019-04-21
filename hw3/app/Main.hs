module Main where

import Interpreter (interpret)
import Parser (codeParse)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

logCode :: String -> IO ()
logCode s = print (s ++ "\n")

main :: IO ()
main = do
  fileName <- fmap head getArgs
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  parsedCode <- codeParse contents
  args <- getArgs
  interpret args parsedCode
