{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Data.Text (Text, pack)
import Text.Megaparsec
    ((<|>), optional, between, choice
    , many, sepBy, sepBy1, Parsec
    , MonadParsec(eof, takeWhileP, try))
import Control.Monad.Combinators.Expr (makeExprParser, Operator(Postfix, InfixL, Prefix))
import Data.Void (Void)
import Ast (Expr(..), Stmt(..), Body, Type(..), ExprWith(ExprWith, getExpr, getInfo))
import Text.Megaparsec.Char (letterChar, alphaNumChar, digitChar, char, string, space)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as H
import Text.Read (readMaybe)
import Data.List (elemIndex)

type Parser = Parsec Void Text

wrap :: Expr () -> ExprWith ()
wrap expr = ExprWith { getExpr = expr, getInfo = () }

keywords :: [String]
keywords = ["If", "Else", "End", "While", "For", "Until", "Then", "By", "To", "Return", "Func", "Break", "Let", "Const", "As"]

identP :: Parser Text
identP = do
    name <- (:) <$> letterChar <*> many alphaNumChar
    if name `elem` keywords
        then fail $ "Unexpected keyword " ++ name
        else return (pack name)

varP, numberP, strlitP, parensP, listP, mapP, termP :: Parser (ExprWith ())
varP = wrap . Variable <$> identP
numberP = do
    number <- many digitChar
    case readMaybe number of
        Just n -> return $ wrap $ Number n
        Nothing -> fail "Not a number"
strlitP = wrap . String <$> between (char '\"') (char '\"') (takeWhileP Nothing (/= '\"'))
parensP = between (symbol "(") (symbol ")") exprP
listP = wrap . List <$> between (symbol "[") (symbol "]") (exprP `sepBy` symbol ",")
mapP = undefined -- TODO:
termP = choice [parensP, strlitP, try varP, listP, numberP] 

operatorTable :: [[Operator Parser (ExprWith ())]]
operatorTable =
  [ [callP]
  , [unop "-", unop "~", unop "not"]
  , [binop "**"]
  , [binop "*", binop "/", binop "%"]
  , [binop "+", binop "-"]
  , [binop "<<", binop ">>"]
  , [binop "&", binop "|", binop "^"]
  , [binop "and", binop "or"]
  , [binop "==", binop "!=", binop "<", binop ">", binop "<=", binop ">="]
  , [binop "="]
  ]
  where
    binop :: Text -> Operator Parser (ExprWith ())
    binop name = InfixL $ (\lhs rhs -> wrap $ Binary lhs rhs name) <$ symbol name
    unop :: Text -> Operator Parser (ExprWith ())
    unop name = Prefix $ wrap . flip Unary name <$ symbol name
    callP :: Operator Parser (ExprWith ())
    callP = Postfix . (id =<<) $ (wrap .) . flip Call <$> (exprP `sepBy` symbol "," <* symbol ")") <$ symbol "("

exprP :: Parser (ExprWith ())
exprP = makeExprParser termP operatorTable

bodyP :: Parser (Body ())
bodyP = many (try $ space >> stmtP)

symbol :: Text -> Parser Text
symbol = try . between space space . string
semicolon :: Parser ()
semicolon = symbol ";" >> return ()

typeP :: Parser Type
typeP = choice [funcTypeP, applTypeP, baseTypeP]
    where
        funcTypeP, applTypeP, baseTypeP :: Parser Type
        funcTypeP = Func
            <$> (between (symbol "(") (symbol ")") $ typeP `sepBy` symbol ",")
            <*> (symbol "->" *> typeP)
        applTypeP = try $ Appl
            <$> (baseTypeP <|> applTypeP) <*> (between (symbol "[") (symbol "]") $ typeP `sepBy` symbol ",")
        baseTypeP = Base <$> identP

ifP, returnP, forP, breakP, typedefP, funcP, letP :: Parser (Stmt ())
ifP = If
    <$> ((:)
        <$> ((,) <$> between (symbol "If") (symbol "Then") exprP <*> bodyP)
        <*> many (try elseIfP))
    <*> (optional elseP <* symbol "End")
    where
        elseIfP :: Parser (ExprWith (), Body ())
        elseIfP = (,)
            <$> (symbol "Else" >> between (symbol "If") (symbol "Then") exprP)
            <*> bodyP
        elseP :: Parser (Body ())
        elseP = symbol "Else" >> bodyP
returnP = Return <$> between (symbol "Return") semicolon exprP
forP = For
    <$> (symbol "For" >> identP)
    <*> optional assignmentP
    <*> (symbol "To" >> exprP)
    <*> optional byP
    <*> between (symbol "Then") (symbol "End") bodyP
    where
        assignmentP :: Parser (ExprWith ())
        assignmentP = symbol "=" >> exprP
        byP :: Parser (ExprWith ())
        byP = symbol "By" >> exprP
breakP = symbol "Break" >> semicolon >> return Break
typedefP = do
    name <- symbol "Type" *> identP
    type_params <- fromMaybe [] <$> optional typeParamsP <* symbol "="
    choice $ map ($ name) [groupP, enumP, aliasP]
    where
        typeParamsP :: Parser [Text]
        typeParamsP = symbol "[" *> identP `sepBy1` symbol "," <* symbol "]"
        aliasP, groupP, enumP :: Text -> Parser (Stmt ())
        aliasP name = AliasDef name <$> typeP <* symbol ";" 
        groupP name = GroupDef name
            <$> (symbol "Group" *> (H.fromList <$> (groupFieldP `sepBy` symbol ",")) <* symbol ";")
            <*> return []
        enumP name = EnumDef name <$> (symbol "Enum" *> identP `sepBy` symbol "," <* symbol ";")
        groupFieldP :: Parser (Text, Type)
        groupFieldP = (,) <$> (identP <* symbol "As") <*> typeP
funcP = do
    name <- symbol "Func" *> identP
    type_params <- fromMaybe [] <$> (optional $ between (symbol "[") (symbol "]") $ identP `sepBy` symbol ",")
    params <- H.fromList <$> (between (symbol "(") (symbol ")") $ paramP `sepBy` symbol ",")
    ret_type <- symbol "As" *> typeP
    body <- map (replace_types_in_body type_params) <$> bodyP <* symbol "End"
    return $ FuncDef name (H.map (replace_type_vars type_params) params) (replace_type_vars type_params ret_type) body
    where
        paramP :: Parser (Text, Type)
        paramP = do
            name <- identP <* symbol "As"
            type_ <- typeP
            return (name, type_)
        replace_type_vars :: [Text] -> Type -> Type
        replace_type_vars type_variables (Base name) =
            case name `elemIndex` type_variables of
                Just index -> Var $ index + 1
                Nothing -> (Base name)
        replace_type_vars vars (Func params ret) = Func (map (replace_type_vars vars) params) $ replace_type_vars vars ret
        replace_type_vars vars (Appl base params) = Appl (replace_type_vars vars base) $ map (replace_type_vars vars) params
        replace_type_vars _ t = t
        replace_types_in_body :: [Text] -> Stmt () -> Stmt ()
        replace_types_in_body vars (Let a b annotation c) = Let a b (replace_type_vars vars <$> annotation) c
        -- TODO: Add overload for function defintions
        replace_types_in_body _ s = s
letP = Let
    <$> ((== "Const") <$> (symbol "Let" <|> symbol "Const"))
    <*> identP
    <*> (optional $ symbol "As" *> typeP)
    <*> between (symbol "=") (semicolon) exprP
whileP :: Bool -> Parser (Stmt ())
whileP until = do
    if until then symbol "Until" else symbol "While"
    cond <- exprP <* symbol "Then"
    body <- bodyP <* symbol "End"
    return $ (if until then (Loop True) else (Loop False)) cond body

stmtP :: Parser (Stmt ())
stmtP = choice [ifP, forP, returnP, breakP, typedefP, letP, whileP True, whileP False, funcP, ExprStmt <$> (exprP <* semicolon)]

programP :: Parser [Stmt ()]
programP = many stmtP <* eof