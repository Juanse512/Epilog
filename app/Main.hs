module Main where

import           Parse                         
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )
import System.IO
import           PrettyPrinter
import           Common
import qualified Logic                         as E1

---------------------------------------------------------

main :: IO()
main = do
    putStr "Ingrese un archivo > "
    hFlush stdout
    fp <- getLine
    s <- readFile fp
    let parseado = parse $ lexer s
    case parseado of
      Ok parsedFile -> case E1.eval parsedFile of
                          Right res -> (do putStr $ renderExp res)
                          Left r -> (do putStr $ id "Error > "
                                        print r)
                                      
      Failed s -> putStrLn s
