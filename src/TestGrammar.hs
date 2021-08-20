module Main where

import System.IO
import System.Environment
import System.Exit

import Generated.LexGrammar
import Generated.ParGrammar
import Generated.ErrM
import TypeChecker
import CheckUtils
import AbsGrammar
import Interpreter
import InterpUtils

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


--DEBUG STUFF--
ee = emptyIEnv

env1 = extendVar ee (Id "x") (Int 123) 

env2 = extendFun env1 (DefFun Type_int (Id "main") [ArgDecl Type_int (Id "argc")] [StmBlock []])

env3 = extendVar (callPush env1) (Id "y") (Double 2.31)

env4 = extendVar (newScope env3) (Id "z") (Bool True)