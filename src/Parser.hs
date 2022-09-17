{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Data.Text (Text, pack)
import Text.Megaparsec
    ((<|>), optional, between, choice
    , many, sepBy, sepBy1, Parsec, SourcePos(..)
    , MonadParsec(eof, takeWhileP, try), some, getSourcePos, mkPos, unPos)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(Postfix, InfixL, Prefix))
import Data.Void (Void)
import Ast (Expr(..), Stmt(..), Body, Type(..), IfMatchBody (IfMatchBody), LetBinding (..))
import Text.Megaparsec.Char (letterChar, alphaNumChar, digitChar, char, string, space, asciiChar)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as H
import Text.Read (readMaybe)
import Data.List (elemIndex)
import Data.Bifunctor (Bifunctor(first), second)
import Error (Span(..))

type Parser = Parsec Void Text

keywords :: [String]
keywords = [ "If", "Else", "End", "While", "For", "Until"
           , "Then", "By", "To", "Return", "Func", "Break"
           , "Let", "Const", "As", "Match", "Otherwise", "When"
           ]

identP :: Parser Text
identP = do
    name <- (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    if name `elem` keywords
        then fail $ "Unexpected keyword " ++ name
        else return (pack name)

varP, numberP, strlitP, parensP, listP, mapP, termP, charP :: Parser Expr
varP = do
    begin <- getSourcePos
    name <- identP
    end <- getSourcePos
    return $ Variable None (Span begin end) name
numberP = do
    begin <- getSourcePos
    number <- many digitChar
    end <- getSourcePos
    case readMaybe number of
        Just n -> return $ Number None (Span begin end) n
        Nothing -> fail "Not a number"
strlitP = do
    begin <- getSourcePos
    content <- between (char '\"') (char '\"') (takeWhileP Nothing (/= '\"'))
    end <- getSourcePos
    return $ String None (Span begin end) content
parensP = between (symbol "(") (symbol ")") exprP
listP = do
    begin <- getSourcePos
    els <- between (symbol "[") (symbol "]") (exprP `sepBy` symbol ",")
    end <- getSourcePos
    return $ List None (Span begin end) els
mapP = do
    begin <- getSourcePos
    kvpairs <- between (symbol "{") (symbol "}") (mapElP `sepBy1` symbol ",")
    end <- getSourcePos
    return $ Dict None kvpairs (Span begin end)
    where
        mapElP :: Parser (Expr, Expr)
        mapElP = (,) <$> exprP <* symbol ":" <*> exprP
charP = do
    begin <- getSourcePos
    c <- between (char '\'') (char '\'') asciiChar
    end <- getSourcePos
    return $ Char None (Span begin end) c
termP = choice [parensP, strlitP, try varP, listP, numberP, charP, mapP]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [postfixP]
  , [unop "-", unop "~", unop "not"]
  , [binop "**"]
  , [binop "*", binop "/", binop "%"]
  , [binop "+", binop "-"]
  , [binop "<<", binop ">>"]
  , [binop "&", binop "|", binop "^"]
  , [binop "==", binop "!=", binop "<", binop ">", binop "<=", binop ">="]
  , [binop "and", binop "or"]
  , [binop "="]
  ]
  where
    binop :: Text -> Operator Parser Expr
    binop name = InfixL $ (\(begin, op, end) lhs rhs -> Binary None lhs rhs op
        (Span begin end) (Span (startLocation (location lhs)) (endLocation (location rhs))))
            <$> ((,,) <$> getSourcePos <*> symbol name <*> getSourcePos)
    unop :: Text -> Operator Parser Expr
    unop name = Prefix $ (\(begin, end) expr -> Unary None expr name
        (Span begin end) (Span begin (endLocation (location expr))))
            <$> ((,) <$> (getSourcePos <* symbol name) <*> getSourcePos)
    postfixP :: Operator Parser Expr
    postfixP = Postfix $ foldl1 (.) <$> some (callP <|> specialisationP)
    callP, specialisationP :: Parser (Expr -> Expr)
    callP = (\(paren, args, end) callee -> Call None callee args (Span paren paren) (Span (startLocation (location callee)) end))
        <$> do
            leftParenLocation <- symbol "(" >> getSourcePos
            args <- exprP `sepBy` symbol "," <* symbol ")"
            endLocation <- getSourcePos
            return (leftParenLocation, args, endLocation)
    specialisationP = (\(args, pos) base -> Specialisation None base args (Span (startLocation (location base)) pos))
        <$> ((,) <$> between (symbol "[") (symbol "]") (typeP `sepBy` symbol ",")
             <*> getSourcePos)

exprP :: Parser Expr
exprP = makeExprParser termP operatorTable

bodyP :: Parser Body
bodyP = many (try $ space >> stmtP)

symbol :: Text -> Parser Text
symbol = try . between space space . string
semicolon :: Parser ()
semicolon = () <$ symbol ";"

typeP :: Parser Type
typeP = choice [funcTypeP, applTypeP, baseTypeP]
    where
        funcTypeP, applTypeP, baseTypeP :: Parser Type
        funcTypeP = Func
            <$> between (symbol "(") (symbol ")") (map (Nothing,) <$> typeP `sepBy` symbol ",")
            <*> (symbol "->" *> typeP)
        applTypeP = try $ Appl
            <$> (baseTypeP <|> applTypeP) <*> between (symbol "[") (symbol "]") (typeP `sepBy` symbol ",")
        baseTypeP = Base <$> identP

ifP, returnP, forP, breakP, typedefP, funcP, letP, matchP :: Parser Stmt
ifP = do
    begin <- getSourcePos
    cond <- between (symbol "If") (symbol "Then") exprP
    end <- getSourcePos
    first_body <- bodyP
    bodies <- many (try elseIfP)
    else_ <- optional $ (,) <$> (getSourcePos >>= \begin -> return $ Span begin (begin { sourceColumn = mkPos $ 4 + unPos (sourceColumn begin) })) <*> elseP
    symbol "End"
    return $ If (IfMatchBody cond (Span begin end) first_body : bodies) (fst <$> else_) (snd <$> else_)
    where
        elseIfP :: Parser IfMatchBody
        elseIfP = do
            begin <- getSourcePos
            cond <- symbol "Else" >> between (symbol "If") (symbol "Then") exprP
            end <- getSourcePos
            IfMatchBody cond (Span begin end) <$> bodyP
        elseP :: Parser Body
        elseP = symbol "Else" >> bodyP
returnP = Return <$> between (symbol "Return") semicolon exprP
forP = do
    (Variable _ loc name) <- symbol "For" >> varP
    startValue <- optional assignmentP
    endValue <- symbol "To" >> exprP
    byValue <- optional byP
    body <- between (symbol "Then") (symbol "End") bodyP
    return $ For name loc startValue endValue byValue body
    where
        assignmentP, byP :: Parser Expr
        assignmentP = symbol "=" >> exprP
        byP = symbol "By" >> exprP
breakP = symbol "Break" >> semicolon >> return Break
typedefP = do
    begin <- getSourcePos
    name <- symbol "Type" *> identP
    end <- getSourcePos
    type_params <- fromMaybe [] <$> optional typeParamsP <* symbol "="
    choice $ map (\f -> f name (Span begin end)) [groupP, enumP, aliasP]
    where
        typeParamsP :: Parser [Text]
        typeParamsP = symbol "[" *> identP `sepBy1` symbol "," <* symbol "]"
        aliasP, groupP, enumP :: Text -> Span -> Parser Stmt
        aliasP name loc = AliasDef name loc <$> typeP <* symbol ";"
        groupP name loc = GroupDef name loc
            <$> (symbol "Group" *> (H.fromList <$> (groupFieldP `sepBy` symbol ",")) <* symbol ";")
            <*> return []
        enumP name loc = EnumDef name loc <$> (symbol "Enum" *> identP `sepBy` symbol "," <* symbol ";")
        groupFieldP :: Parser (Text, (Type, Span))
        groupFieldP = do
            begin <- getSourcePos
            name <- identP <* symbol "As"
            type_ <- typeP
            end <- getSourcePos
            return (name, (type_, Span begin end))
funcP = do
    symbol "Func"
    nameBegin <- getSourcePos
    name <- identP
    nameEnd <- getSourcePos
    type_params <- fromMaybe [] <$> optional (between (symbol "[") (symbol "]") $ identP `sepBy` symbol ",")
    params <- between (symbol "(") (symbol ")") (paramP `sepBy` symbol ",")
    symbol "As"
    retBegin <- getSourcePos
    ret_type <- typeP
    retEnd <- getSourcePos
    body <- map (replace_types_in_body type_params) <$> bodyP <* symbol "End"
    return $ FuncDef name (Span nameBegin nameEnd) (map (second $ first $ replace_type_vars type_params) params) (replace_type_vars type_params ret_type) (Span retBegin retEnd) body
    where
        paramP :: Parser (Text, (Type, Span))
        paramP = do
            start <- getSourcePos
            name <- identP <* symbol "As"
            type_ <- typeP
            end <- getSourcePos
            return (name, (type_, Span start end))
        replace_type_vars :: [Text] -> Type -> Type
        replace_type_vars type_variables (Base name) =
            case name `elemIndex` type_variables of
                Just index -> Var name (index + 1)
                Nothing -> Base name
        replace_type_vars vars (Func params ret) = Func (map (second $ replace_type_vars vars) params) $ replace_type_vars vars ret
        replace_type_vars vars (Appl base params) = Appl (replace_type_vars vars base) $ map (replace_type_vars vars) params
        replace_type_vars _ t = t
        replace_types_in_body :: [Text] -> Stmt -> Stmt
        replace_types_in_body vars (Let isConst bindings) =
            Let isConst $ map (replaceInBinding vars) bindings
            where
                replaceInBinding :: [Text] -> LetBinding -> LetBinding
                replaceInBinding vars l@(LetBinding _ _ anon _ _) =
                    l { annotation = replace_type_vars vars <$> anon }
        -- TODO: Add overload for function defintions
        replace_types_in_body _ s = s
letP = do
    isConst <- (== "Const") <$> (symbol "Let" <|> symbol "Const")
    bindings <- bindingP `sepBy1` symbol "," <* semicolon
    return $ Let isConst bindings
    where
        bindingP :: Parser LetBinding
        bindingP = do
            nameBegin <- getSourcePos
            name <- identP
            nameEnd <- getSourcePos
            annotation <- optional $ do
                start <- symbol "As" *> getSourcePos
                type_ <- typeP
                end <- getSourcePos
                return (type_, Span start end)
            value <- symbol "=" >> exprP
            return LetBinding { name = name, nameLocation = Span nameBegin nameEnd
                , annotation = fst <$> annotation, annotationLocation = snd <$> annotation, value = value }
matchP = do
    variable <- between (symbol "Match") (symbol "Then") exprP
    blocks <- some blockP
    other <- optional $ between (symbol "Otherwise") (symbol "End") bodyP
    symbol "End"
    return $ Match variable blocks other
    where
        blockP :: Parser IfMatchBody
        blockP = do
            condBegin <- getSourcePos
            cond <- between (symbol "When") (symbol "Then") exprP 
            condEnd <- getSourcePos
            body <- bodyP <* symbol "End"
            return $ IfMatchBody cond (Span condBegin condEnd) body
whileP :: Bool -> Parser Stmt
whileP until = do
    if until then symbol "Until" else symbol "While"
    cond <- exprP <* symbol "Then"
    body <- bodyP <* symbol "End"
    return $ (if until then Loop True else Loop False) cond body

stmtP :: Parser Stmt
stmtP = choice [ifP, forP, returnP, breakP, typedefP, letP, matchP, whileP True, whileP False, funcP, ExprStmt <$> (exprP <* semicolon)]

programP :: Parser [Stmt]
programP = many stmtP <* eof