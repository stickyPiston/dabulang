{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Std where

import Ast
    ( Type(Base, Var, Appl, Func),
      Value(VIntrin, VString, VList, VNat, VInt, VChar, VBool, VSole),
      extractChar)
import Types (Env (..))
import qualified Data.HashMap.Strict as M
import Data.Text (pack, Text, unpack)
import Text.Read (readMaybe)
import Data.Char ( ord, chr )
import Control.Monad.IO.Class (liftIO, MonadIO)

env :: Env
env = Env { gamma = M.fromList [
        ("length", (True, Func [(Nothing, Appl (Base "List") [Var "T" 1])] (Base "Nat"))),
        ("print", (True, Func [(Nothing, Var "T" 1)] (Base "Trivial"))),
        ("input", (True, Func [] (Base "String"))),
        ("put_char", (True, Func [(Nothing, Base "Char")] (Base "Trivial"))),
        ("read", (True, Func [(Nothing, Base "String")] (Base "Nat"))),
        ("True", (True, Base "Bool")),
        ("False", (True, Base "Bool")),
        ("cons", (True, Func [(Nothing, Var "T" 1), (Nothing, Appl (Base "List") [Var "T" 1])] (Appl (Base "List") [Var "T" 1]))),
        ("sole", (True, Base "Trivial")),
        ("to_char_list", (True, Func [(Nothing, Base "String")] (Appl (Base "List") [Base "Char"]))),
        ("to_string", (True, Func [(Nothing, (Appl (Base "List") [Base "Char"]))] (Base "String"))),
        ("to_ascii", (True, Func [(Nothing, Base "Char")] (Base "Nat"))),
        ("from_ascii", (True, Func [(Nothing, Base "Nat")] (Base "Char"))),
        ("read_file", (True, Func [(Nothing, Base "String")] (Appl (Base "List") [Base "String"])))
    ], delta = M.empty, currentFunction = "" }

prelude :: M.HashMap Text Value
prelude = M.fromList [
        ("length", VIntrin intrin_length)
    ,   ("read", VIntrin intrin_read)
    ,   ("print", VIntrin intrin_print)
    ,   ("input", VIntrin intrin_input)
    ,   ("put_char", VIntrin intrin_put_char)
    ,   ("cons", VIntrin intrin_cons)
    ,   ("True", VBool True)
    ,   ("False", VBool False)
    ,   ("sole", VSole)
    ,   ("to_char_list", VIntrin intrin_char_list)
    ,   ("to_string", VIntrin intrin_to_string)
    ,   ("to_ascii", VIntrin intrin_to_ascii)
    ,   ("from_ascii", VIntrin intrin_from_ascii)
    ,   ("read_file", VIntrin intrin_read_file)
    ]
    where
        intrin_length [a] = case a of
            VList vs -> return $ VNat $ length vs
        intrin_read [s] = case s of
            VString s -> case readMaybe (unpack s) of
              Nothing -> error $ unpack s
              Just any -> return $ VNat any
        intrin_print [t] = liftIO $ print t >> return VSole
        intrin_input _ = liftIO $ getLine >>= \s -> return $ VString $ pack s
        intrin_put_char [c] = liftIO $ case c of
            VChar c -> putChar c >> return VSole
        intrin_cons [a, l] = case l of
            VList as -> return . VList $ a : as
        intrin_char_list [l] = case l of
            VString s -> return $ VList $ map VChar $ unpack s
        intrin_to_string [s] = case s of
            VList s -> return $ VString $ pack $ map extractChar s
        intrin_to_ascii [c] = case c of VChar c -> return $ VNat $ ord c
        intrin_from_ascii [n] = case n of
            VNat n -> return $ VChar $ chr n
            VInt n -> return $ VChar $ chr n
        intrin_read_file [f] = case f of
            VString f -> liftIO $ readFile (unpack f) >>= \l -> return $ VList $ map (VString . pack) (lines l)