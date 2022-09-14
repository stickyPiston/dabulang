{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser (programP)
import Text.Megaparsec (parseTest, parse, errorBundlePretty)
import System.Environment (getArgs)
import Data.Text (pack, Text)
import Types (Env(Env, gamma, delta), inferStmt)
import Control.Monad.State (StateT(runStateT), evalStateT, MonadIO (liftIO), MonadTrans (lift))
import Data.HashMap.Strict (empty, fromList)
import Ast (Type(Func, Base, Appl, Var))
import Error (printError, Error (ParseError, ICE))
import Eval (evalStmt, prelude, evalProgram)
import Control.Monad.Trans.Except (runExceptT, ExceptT (ExceptT), except, withExceptT)
import qualified Data.Text.IO as Text

env :: Env
env = Env { gamma = fromList [
        ("length", (True, Func [(Nothing, Appl (Base "List") [Var "T" 1])] (Base "Nat"))),
        ("print", (True, Func [(Nothing, Var "T" 1)] (Base "Trivial"))),
        ("put_char", (True, Func [(Nothing, Base "Char")] (Base "Trivial"))),
        ("read", (True, Func [(Nothing, Base "String")] (Base "Nat"))),
        ("True", (True, Base "Bool")),
        ("False", (True, Base "Bool")),
        ("cons", (True, Func [(Nothing, Var "T" 1), (Nothing, Appl (Base "List") [Var "T" 1])] (Appl (Base "List") [Var "T" 1]))),
        ("sole", (True, Base "Trivial")),
        ("to_char_list", (True, Func [(Nothing, Base "String")] (Appl (Base "List") [Base "Char"]))),
        ("to_ascii", (True, Func [(Nothing, Base "Char")] (Base "Nat"))),
        ("from_ascii", (True, Func [(Nothing, Base "Nat")] (Base "Char")))
    ], delta = empty }

main :: IO ()
main = do
    args <- getArgs
    result <- case args of
        [filePath] ->
            Text.readFile filePath >>= \source -> runExceptT $ withExceptT (printError source) $ do
                ast <- except $ mapLeft ParseError $ parse programP filePath source
                sts <- except $ evalStateT (mapM inferStmt ast) env
                except =<< liftIO (evalStateT (runExceptT $ evalProgram sts) prelude)
        _ -> return $ Left "Invalid arguments"
    either Text.putStrLn (const $ return ()) result
    where
        mapLeft :: (a -> c) -> Either a b -> Either c b
        mapLeft f (Left a) = Left $ f a
        mapLeft _ (Right b) = Right b