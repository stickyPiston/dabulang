{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Std where

import Ast
    ( Type(Base, Var, Appl, Func),
      Value(VIntrin, VString, VList, VNat, VInt, VChar, VBool, VSole) )
import Types (Env (..))
import qualified Data.HashMap.Strict as M
import Data.Text (Text, unpack)
import Text.Read (readMaybe)
import Data.Char ( ord, chr )
import Control.Monad.IO.Class (liftIO, MonadIO)

env :: Env
env = Env { gamma = M.fromList [
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
    ], delta = M.empty }

prelude :: M.HashMap Text Value
prelude = M.fromList [
        ("length", VIntrin intrin_length)
    ,   ("read", VIntrin intrin_read)
    ,   ("print", VIntrin intrin_print)
    ,   ("put_char", VIntrin intrin_put_char)
    ,   ("cons", VIntrin intrin_cons)
    ,   ("True", VBool True)
    ,   ("False", VBool False)
    ,   ("sole", VSole)
    ,   ("to_char_list", VIntrin intrin_char_list)
    ,   ("to_ascii", VIntrin intrin_to_ascii)
    ,   ("from_ascii", VIntrin intrin_from_ascii)
    ]
    where
        intrin_length [a] = case a of
            VList vs -> return $ VNat $ length vs
        intrin_read [s] = case s of
            VString s -> case readMaybe (unpack s) of
              Nothing -> error $ unpack s
              Just any -> return $ VNat any
        intrin_print [t] = liftIO $ print t >> return VSole
        intrin_put_char [c] = liftIO $ case c of
            VChar c -> putChar c >> return VSole
        intrin_cons [a, l] = case l of
            VList as -> return . VList $ a : as
        intrin_char_list [l] = case l of
            VString s -> return $ VList $ map VChar $ unpack s
        intrin_to_ascii [c] = case c of VChar c -> return $ VNat $ ord c
        intrin_from_ascii [n] = case n of
            VNat n -> return $ VChar $ chr n
            VInt n -> return $ VChar $ chr n
