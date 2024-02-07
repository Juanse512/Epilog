module Main where

import           Parse                         
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )
import           PrettyPrinter
import           Common
import qualified Logic                         as E1

---------------------------------------------------------

-- data Options = Options
--   { optPrint :: Bool
--   , optAST   :: Bool
--   , optEval  :: Int
--   , optHelp  :: Bool
--   }
--   deriving Show

-- defaultOptions :: Options
-- defaultOptions =
--   Options { optPrint = False, optAST = False, optEval = 0, optHelp = False }

-- options :: [OptDescr (Options -> Options)]
-- options =
--   [ Option ['p']
--            ["print"]
--            (NoArg (\opts -> opts { optPrint = True }))
--            "Imprimir el programa de entrada."
--   , Option ['a']
--            ["AST"]
--            (NoArg (\opts -> opts { optAST = True }))
--            "Mostrar el AST del programa de entrada."
--   , Option ['e']
--            ["evaluator"]
--            (ReqArg (\s opts -> opts { optEval = read s }) "N_EVALUADOR")
--            "Elegir evaluador 1, 2 o 3."
--   , Option ['h']
--            ["help"]
--            (NoArg (\opts -> opts { optHelp = True }))
--            "Imprimir guia de uso."
--   ]

-- finalOptions :: [String] -> IO (Options, [String])
-- finalOptions argv = case getOpt Permute options argv of
--   (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
--   (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
--   where header = "Uso:"

-- main :: IO ()
-- main = do
--   s : opts   <- Env.getArgs
--   (opts', _) <- finalOptions opts
--   runOptions s opts'

-- runOptions :: FilePath -> Options -> IO ()
-- runOptions fp opts
--   | optHelp opts = putStrLn (usageInfo "Uso: " options)
--   | otherwise = do
--     s <- readFile fp
--     case parseComm fp s of
--       Left  error -> print error
--       Right ast   -> if
--         | optAST opts       -> print ast
--         | optPrint opts     -> putStrLn (renderComm ast)
--         | optEval opts == 1 -> print (E1.eval ast)
--         | optEval opts == 2 -> print (E2.eval ast)
--         | optEval opts == 3 -> print (E3.eval ast)
--         | otherwise         -> print (E1.eval ast)


main :: IO()
main = do
    fp <- getLine
    s <- readFile fp
    putStrLn s
    putStrLn $ show (lexer s)
    putStrLn "Enter parse"
    let parseado = parse $ lexer s
    putStrLn "Exit parse"
    putStrLn $ show (parseado)
    case parseado of
      -- Ok parsedFile -> case parsedFile of
      --                     Def s e -> print (E1.eval e)
      --                     Eval e -> print (E1.eval e)
      Ok parsedFile -> do putStrLn "Entering eval"
                          print (E1.eval parsedFile)
                          putStrLn "Exiting eval"            
      Failed s -> putStrLn s
      -- Left error -> print error
      -- Right ast -> print (E1.eval ast)