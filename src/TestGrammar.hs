module Main where

import System.IO
import System.Environment
import System.Exit

import Generated.LexGrammar
import Generated.ParGrammar
import Generated.ErrM
import TypeChecker
import AbsGrammar
import CheckUtils

myLLexer = myLexer

type DebugMode = Bool

putStrMode :: DebugMode -> String -> IO ()
putStrMode mode s = if mode == True then putStrLn s else return ()

runFile :: DebugMode -> FilePath -> IO ()
runFile mode f = putStrLn f >> readFile f >>= run mode

run :: DebugMode -> String -> IO ()
run mode s = let ast = pProgram (myLLexer s) in case ast of
      Bad err -> do 
            putStrLn "Parser: ERROR"
            putStrLn err
            exitFailure
      Ok tree -> do 
            putStrLn "Parser: OK"
            case checkProgram tree of
                  Bad err -> do
                        putStrLn "Typechecker: ERROR"
                        putStrLn err
                        exitFailure
                  Ok _ -> do
                        putStrLn "Typechecker: OK"
                        showTree mode tree
                        exitSuccess


showTree :: (Show a) => DebugMode -> a -> IO ()
showTree mode tree = do
  putStrMode mode $ "\n[Abstract Syntax Tree]\n\n" ++ show tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run True -- read from stdin
    fs -> mapM_ (runFile True) fs         -- read from file







--debug stuff
env::Env
env = emptyEnv

env1::Env
env1 = updateVar env (Id "val") Type_int

env2::Env
env2 = updateVar (updateFun env1 (Id "main") ([Type_bool, Type_int], Type_void)) (Id "a") Type_bool

env3 = updateVar (newBlock (updateVar (newBlock env2) (Id "b") Type_double)) (Id "arr") Type_void