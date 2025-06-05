module Main where

import FromHs (fromHsString)
import Reduce (reduce)
import Syntax (Prog (progDefs))
import System.Environment (getArgs)

dividerLen :: Int
dividerLen = 60

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help", filename] -> printHelp >> readFile filename >>= runProgString
    ["--help"] -> printHelp
    [filename] -> readFile filename >>= runProgString
    [] -> getContents >>= runProgString
    _ -> printHelp

runProgString :: String -> IO ()
runProgString progString = do
  let prog = fromHsString progString
  mapM_ print (progDefs prog)
  putStrLn (replicate dividerLen '-')
  mapM_ putStrLn (reduce prog)

printHelp :: IO ()
printHelp =
  putStrLn
    "Usage: zadanie2 [--help] [file]\n\
    \  --help  - display this message\n\
    \  file    - file with program to reduce"
