{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ast where

import qualified Data.HashMap.Strict as M
import Data.Text (Text, intercalate, pack, unpack)
import TextShow (TextShow(showb, showt), toText, fromString, fromText, Builder)
import Data.HashMap.Strict (mapWithKey, elems)
import Text.Megaparsec (SourcePos)

data Type
    = Base { nameT :: Text }
    | Func { paramTypes :: [(Maybe Text, Type)], retType :: Type }
    | Appl { base :: Type, argTypes :: [Type] }
    | Var { nameT :: Text, index :: Int }
    | Group { nameT :: Text, fieldTypes :: M.HashMap Text Type, extends :: [Text] }
    | Enum { nameT :: Text, names :: [Text] }
    | Alias { nameT :: Text, aliasee :: Type }
    | None
    deriving Eq
instance TextShow Type where
    showb (Base name) = fromText name
    showb (Func params ret) = "(" <> intercalateBuilder ", " (map (showb . snd) params) <> ") -> " <> showb ret
    showb (Appl base args) = showb base <> "[" <> intercalateBuilder ", " (map showb args) <> "]"
    showb (Var name _) = fromText name
    showb (Group _ fields supers) = "Group " <> (intercalateBuilder ", " . M.elems $ M.mapWithKey (\k v -> showb k <> " As " <> showb v) fields)
    showb (Enum _ fields) = "Enum " <> showb (intercalate "," fields)
    showb (Alias name aliasee) = showb name <> " => " <> showb aliasee
    showb None = "None"
instance Show Type where show = unpack . showt

data Span = Span { startLocation :: SourcePos, endLocation :: SourcePos }
    deriving Show

data IfMatchBody = IfMatchBody { cond :: Expr, condLocation :: Span, body :: Body }
    deriving Show
data LetBinding = LetBinding { name :: Text, nameLocation :: Span, annotation :: Maybe Type
    , annotationLocation :: Maybe Span, value :: Expr }
instance TextShow LetBinding where
    showb (LetBinding name _ anon _ val) = fromText name <> maybe "" ((" As " <>) . showb) anon <> " = " <> showb val

type Body = [Stmt]
data Stmt
    = If { bodies :: [IfMatchBody], elseLocation :: Maybe Span, elseProg :: Maybe Body }
    | Loop { isUntil :: Bool, condU :: Expr, bodyU :: Body }
    | Return { value :: Expr }
    | Break
    | GroupDef { name :: Text, nameLocation :: Span, fields :: M.HashMap Text (Type, Span)
               , extends :: [(Text, Span)] }
    | EnumDef { name :: Text, nameLocation :: Span, values :: [Text] }
    | AliasDef { name :: Text, nameLocation :: Span, aliasee :: Type }
    | FuncDef { name :: Text, nameLocation :: Span, params :: [(Text, (Type, Span))]
              , retType :: Type, retTypeLocation :: Span, bodyFu :: Body }
    | For { variable :: Text, variableLocation :: Span, startValue :: Maybe Expr
          , endValue :: Expr, byValue :: Maybe Expr, bodyFo :: Body }
    | Match { value :: Expr, bodies :: [IfMatchBody]
            , other :: Maybe Body }
    | Let { isConst :: Bool, bindings :: [LetBinding] }
    | ExprStmt { expr :: Expr }
instance TextShow Stmt where
    showb (If ((IfMatchBody cond _ body) : bodies) _ else_) = "If " <> showb cond <> " Then " <> showb body
        <> mconcat (map (\(IfMatchBody cond _ body) -> "Else If " <> showb cond <> " Then " <> showb body) bodies)
        <> maybe "" (("Else" <>) . showb) else_
        <> " End"
    showb (Loop is_until cond body) = (if is_until then "Until " else "While ")
        <> showb cond <> " Then " <> showb body <> " End"
    showb (Return expr) = "Return " <> showb expr <> ";"
    showb Break = "Break"
    showb (FuncDef name _ params ret_type _ body) = "Func " <> fromText name <> "("
        <> intercalateBuilder ", " (map (\(k, (v, _)) -> fromText k <> " As " <> showb v) params) <> ")"
        <> " As " <> showb ret_type <> " " <> showb body <> " End"
    showb (For var _ start end by body) = "For " <> fromText var <> maybe "" ((" = " <>) . showb) start
        <> " To " <> showb end <> maybe "" ((" By " <>) . showb) by <> " Then " <> showb body <> " End"
    showb (Let is_const bindings) = (if is_const then "Const " else "Let ") <>
        intercalateBuilder ", " (map showb bindings) <> ";"
    showb (ExprStmt expr) = showb expr <> ";"
instance Show Stmt where show = unpack . showt

data Expr
    = Number { type_ :: Type, location :: Span, nValue :: Int }
    | String { type_ :: Type, location :: Span, sValue :: Text }
    | Variable { type_ :: Type, location :: Span, name :: Text }
    | Binary { type_ :: Type, lhs :: Expr, rhs :: Expr, op :: Text, opLocation :: Span, location :: Span }
    | Unary { type_ :: Type, rhs :: Expr, op :: Text, opLocation :: Span, location :: Span }
    | Call { type_ :: Type, callee :: Expr, args :: [Expr], leftParenLocation :: Span, location :: Span }
    | List { type_ :: Type, location :: Span, els :: [Expr] }
    | Dict { type_ :: Type, kvpairs :: [(Expr, Expr)], location :: Span }
    | Specialisation { type_ :: Type, base :: Expr, types :: [Type], location :: Span }
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
instance Show Expr where show = unpack.showt

