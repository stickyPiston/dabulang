{-# LANGUAGE OverloadedStrings #-}

module Ast where

import qualified Data.HashMap.Strict as M
import Data.Text (Text, intercalate, pack, unpack)
import TextShow (TextShow(showb, showt), toText, fromString, fromText, Builder)
import Data.HashMap.Strict (mapWithKey, elems)

data Type
    = Base Text
    | Func [Type] Type
    | Appl Type [Type]
    | Var Int
    | Group (M.HashMap Text Type) [Type]
    | Enum [Text]
    | Alias Text Type
    deriving Eq
instance TextShow Type where
    showb (Base name) = fromText name
    showb (Func params ret) = "(" <> (intercalateBuilder ", " $ map showb params) <> ") -> " <> showb ret
    showb (Appl base args) = showb base <> "[" <> (intercalateBuilder ", " $ map showb args) <> "]"
    showb (Var i) = "$" <> showb i
    showb (Group fields supers) = "Group " <> (intercalateBuilder ", " . M.elems $ M.mapWithKey (\k v -> showb k <> " As " <> showb v) fields)
    showb (Enum fields) = "Enum " <> showb (intercalate "," fields)
    showb (Alias name aliasee) = showb name <> " => " <> showb aliasee
instance Show Type where show = unpack . showt

type Body i = [Stmt i]
data Stmt i
    = If [(ExprWith i, Body i)] (Maybe (Body i))
    | Loop Bool (ExprWith i) (Body i)
    | Return (ExprWith i)
    | Break
    | GroupDef Text (M.HashMap Text Type) [Text]
    | EnumDef Text [Text]
    | AliasDef Text Type
    | FuncDef Text (M.HashMap Text Type) Type (Body i)
    | For Text (Maybe (ExprWith i)) (ExprWith i) (Maybe (ExprWith i)) (Body i)
    | Match (ExprWith i) [(ExprWith i, Body i)] (Maybe (Body i))
    | Let Bool Text (Maybe Type) (ExprWith i)
    | ExprStmt (ExprWith i)
instance TextShow i => TextShow (Stmt i) where
    showb (If ((cond, body) : bodies) else_) = "If " <> showb cond <> " Then " <> showb body
        <> mconcat (map (\(cond, body) -> "Else If " <> showb cond <> " Then " <> showb body) bodies)
        <> maybe "" (("Else" <>) . showb) else_
        <> " End"
    showb (Loop is_until cond body) = (if is_until then "Until " else "While ")
        <> showb cond <> " Then " <> showb body <> " End"
    showb (Return expr) = "Return " <> showb expr <> ";"
    showb Break = "Break"
    showb (FuncDef name params ret_type body) = "Func " <> fromText name <> "("
        <> intercalateBuilder ", " (elems $ mapWithKey (\k v -> fromText k <> " As " <> showb v) params) <> ")"
        <> " As " <> showb ret_type <> " " <> showb body <> " End"
    showb (For var start end by body) = "For " <> fromText var <> (maybe "" ((" = " <>) . showb) start)
        <> " To " <> showb end <> (maybe "" ((" By " <>) . showb) by) <> " Then " <> showb body <> " End"
    showb (Let is_const name type_ expr) = (if is_const then "Const " else "Let ") <> fromText name
        <> (maybe "" ((" As " <>) . showb) type_) <> " = " <> showb expr <> ";"
    showb (ExprStmt expr) = showb expr <> ";"
instance (TextShow i, Show i) => Show (Stmt i) where show = unpack . showt

data Expr i
    = Number Int
    | String Text
    | Variable Text
    | Binary (ExprWith i) (ExprWith i) Text
    | Unary (ExprWith i) Text
    | Call (ExprWith i) [ExprWith i]
    | List [ExprWith i]
    | Dict [(ExprWith i, ExprWith i)]
    | Specialisation (ExprWith i) [Type]
intercalateBuilder :: Builder -> [Builder] -> Builder
intercalateBuilder _ [] = fromText ""
intercalateBuilder t (e : []) = e
intercalateBuilder t (e : es) = e <> t <> intercalateBuilder t es
instance TextShow i => TextShow (Expr i) where
    showb (Number n) = showb n
    showb (String s) = "\"" <> fromText s <> "\""
    showb (Variable name) = fromText name
    showb (Binary l r o) = showb l <> " " <> fromText o <> " " <> showb r
    showb (Unary l o) = fromText o <> showb l
    showb (Call callee args) = showb callee <> "(" <> mconcat (map showb args) <> ")"
    showb (List els) = "[" <> intercalateBuilder "," (map showb els) <> "]"
    showb (Dict els) = "{" <> intercalateBuilder "," (map (\(k, v) -> showb k <> " : " <> showb v) els) <> "}"
    showb (Specialisation base args) = showb args <> "[" <> intercalateBuilder "," (map showb args) <> "]"
data ExprWith i = ExprWith { getExpr :: Expr i , getInfo :: i }
instance TextShow i => TextShow (ExprWith i) where
    showb ExprWith { getExpr = getExpr } = showb getExpr
