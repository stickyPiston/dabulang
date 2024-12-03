{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ast where

import qualified Data.HashMap.Strict as M
import Data.Text (Text, intercalate, pack, unpack)
import TextShow (TextShow(showb, showt), toText, fromString, fromText, Builder)
import Data.HashMap.Strict (mapWithKey, elems)
import Text.Megaparsec (SourcePos)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Error (Error, Span)
import qualified Data.HashMap.Internal.Strict as H

data Type
    = Base { nameT :: Text }
    | Func { paramTypes :: [(Maybe Text, Type)], retType :: Type }
    | Appl { base :: Type, argTypes :: [Type] }
    | Var { nameT :: Text, index :: Int }
    | None
    deriving Eq
instance TextShow Type where
    showb (Base name) = fromText name
    showb (Func params ret) = "(" <> intercalateBuilder ", " (map (showb . snd) params) <> ") -> " <> showb ret
    showb (Appl base args) = showb base <> "[" <> intercalateBuilder ", " (map showb args) <> "]"
    showb (Var name _) = fromText name
    showb None = "None"

data IfMatchBody = IfMatchBody { cond :: Expr, condLocation :: Span, body :: Body, bodySpan :: Span }
data LetBinding = LetBinding { name :: Text, nameLocation :: Span, annotation :: Maybe Type
    , annotationLocation :: Maybe Span, value :: Expr }
instance TextShow LetBinding where
    showb (LetBinding name _ anon _ val) = fromText name <> maybe "" ((" As " <>) . showb) anon <> " = " <> showb val

type Body = [Stmt]
data Stmt
    = If { bodies :: [IfMatchBody], elseLocation :: Maybe Span, elseProg :: Maybe Body, elseProgSpan :: Maybe Span }
    | Loop { isUntil :: Bool, condU :: Expr, bodyU :: Body, bodyUSpan :: Span }
    | Return { value :: Expr }
    | Break
    | FuncDef { name :: Text, nameLocation :: Span, params :: [(Text, (Type, Span))], typeF_ :: Type
              , retType :: Type, retTypeLocation :: Span, bodyFu :: Body, defSpan :: Span }
    | For { variable :: Text, variableLocation :: Span, startValue :: Maybe Expr
          , endValue :: Expr, byValue :: Maybe Expr, bodyFo :: Body, bodyFSpan :: Span }
    | Match { value :: Expr, bodies :: [IfMatchBody]
            , other :: Maybe Body, otherSpan :: Maybe Span }
    | Let { isConst :: Bool, bindings :: [LetBinding] }
    | ExprStmt { expr :: Expr }
instance TextShow Stmt where
    showb (If ((IfMatchBody cond _ body _) : bodies) _ else_ _) = "If " <> showb cond <> " Then " <> showb body
        <> mconcat (map (\(IfMatchBody cond _ body _) -> "Else If " <> showb cond <> " Then " <> showb body) bodies)
        <> maybe "" (("Else" <>) . showb) else_
        <> " End"
    showb (Loop is_until cond body span) = (if is_until then "Until " else "While ")
        <> showb cond <> " Then " <> showb body <> " End"
    showb (Return expr) = "Return " <> showb expr <> ";"
    showb Break = "Break"
    showb (FuncDef name _ params _ ret_type _ body _) = "Func " <> fromText name <> "("
        <> intercalateBuilder ", " (map (\(k, (v, _)) -> fromText k <> " As " <> showb v) params) <> ")"
        <> " As " <> showb ret_type <> " " <> showb body <> " End"
    showb (For var _ start end by body _) = "For " <> fromText var <> maybe "" ((" = " <>) . showb) start
        <> " To " <> showb end <> maybe "" ((" By " <>) . showb) by <> " Then " <> showb body <> " End"
    showb (Let is_const bindings) = (if is_const then "Const " else "Let ") <>
        intercalateBuilder ", " (map showb bindings) <> ";"
    showb (ExprStmt expr) = showb expr <> ";"

data Expr
    = Number { type_ :: Type, location :: Span, nValue :: Int }
    | String { type_ :: Type, location :: Span, sValue :: Text }
    | Variable { type_ :: Type, location :: Span, name :: Text }
    | Binary { type_ :: Type, lhs :: Expr, rhs :: Expr, op :: Text, opLocation :: Span, location :: Span }
    | Unary { type_ :: Type, rhs :: Expr, op :: Text, opLocation :: Span, location :: Span }
    | Call { type_ :: Type, callee :: Expr, args :: [Expr], leftParenLocation :: Span, location :: Span }
    | List { type_ :: Type, location :: Span, els :: [Expr] }
    | Dict { type_ :: Type, kvpairs :: [(Expr, Expr)], location :: Span }
    | Specialisation { type_ :: Type, base :: Expr, typeparams :: [Type], location :: Span }
    | Char { type_ :: Type, location :: Span, cValue :: Char }

intercalateBuilder :: Builder -> [Builder] -> Builder
intercalateBuilder _ [] = fromText ""
intercalateBuilder t [e] = e
intercalateBuilder t (e : es) = e <> t <> intercalateBuilder t es
instance TextShow Expr where
    showb (Number _ _ n) = showb n
    showb (String _ _ s) = "\"" <> fromText s <> "\""
    showb (Variable _ _ name) = fromText name
    showb (Binary _ l r o _ _) = showb l <> " " <> fromText o <> " " <> showb r
    showb (Unary _ l o _ _) = fromText o <> showb l
    showb (Call _ callee args _ _) = showb callee <> "(" <> intercalateBuilder ", " (map showb args) <> ")"
    showb (List _ _ els) = "[" <> intercalateBuilder "," (map showb els) <> "]"
    showb (Dict _ els _) = "{" <> intercalateBuilder "," (map (\(k, v) -> showb k <> " : " <> showb v) els) <> "}"
    showb (Specialisation _ base args _) = showb base <> "[" <> intercalateBuilder ", " (map showb args) <> "]"
    showb (Char _ _ c) = "'" <> fromString [c] <> "'"

data Value
    = VBool { extractBool :: Bool }
    | VInt { extractInt :: Int }
    | VNat { extractNat :: Int }
    | VString { extractString :: Text }
    | VFunc [Text] Body
    | VIntrin ([Value] -> Eval Value)
    | VSole
    | VList [Value]
    | VMap [(Value, Value)]
    | VChar { extractChar :: Char }
extractNumber :: Value -> Int
extractNumber (VNat n) = n
extractNumber (VInt n) = n
extractNumber e = error $ unpack $ "Extracting number from " <> showt e
instance Eq Value where
    VBool a == VBool b = a == b
    VInt a == VInt b = a == b
    VNat a == VNat b = a == b
    VInt a == VNat b = a == b
    VNat a == VInt b = a == b
    VString a == VString b = a == b
    VSole == VSole = True
    VList a == VList b = a == b
    VMap a == VMap b = a == b
    VChar a == VChar b = a == b
    a == b = False
instance TextShow Value where
    showb (VBool b) = showb b
    showb (VInt n) = showb n
    showb (VNat n) = showb n
    showb (VString s) = showb s
    showb VSole = "sole"
    showb (VList els) = "[" <> intercalateBuilder ", " (map showb els) <> "]"
    showb (VChar c) = "'" <> fromString [c] <> "'"
    showb (VMap els) = "{" <> intercalateBuilder ", " (map (\(k, v) -> showb k <> " : " <> showb v) els) <> "}"
    showb a = "thing"

data RuntimeError = Error Error | ReturnValue Value | BreakLoop
data EvalState = EvalState { variables :: M.HashMap Text Value, types :: M.HashMap Text Type }
type Eval = ExceptT RuntimeError (StateT EvalState IO)