{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser (programP)
import Text.Megaparsec (parseTest, parse, errorBundlePretty)
import System.Environment (getArgs)
import Data.Text (pack)
import Types (Env(Env, gamma, delta), infer_stmt)
import Control.Monad.State (StateT(runStateT), evalStateT)
import Data.HashMap.Strict (empty, fromList)
import Ast (Type(Func, Base, Appl, Var))

env :: Env
env = Env { gamma = fromList [("length", (True, Func [Appl (Base "List") [Var 1]] (Base "Nat")))], delta = empty }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file_path] -> do
            source <- readFile file_path
            case parse programP file_path (pack source) of
                Left e -> putStr $ errorBundlePretty e
                Right ast -> print $ evalStateT (sequence $ map infer_stmt ast) env
        _ -> print "Invalid set of arguments"