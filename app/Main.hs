{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser (programP)
import Text.Megaparsec (parse)
import System.Environment (getArgs)
import Types (Env(Env, gamma, delta), inferStmt)
import Control.Monad.State (StateT(runStateT), evalStateT, MonadIO (liftIO), MonadTrans (lift))
import Data.HashMap.Strict (empty, fromList)
import Error (printError, Error (ParseError, ICE))
import Eval (evalProgram)
import Std ( env, prelude )
import Control.Monad.Trans.Except (runExceptT, ExceptT (ExceptT), except, withExceptT)
import qualified Data.Text.IO as Text

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