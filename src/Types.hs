{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Types(inferStmt, Env(..)) where

import qualified Data.HashMap.Strict as M
import Data.List (intercalate, find, transpose, sort, nub, zip4)
import Data.Either (rights, lefts, isLeft)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans (lift)
import Control.Monad.State (modify, StateT, MonadState (get, put), when, zipWithM)
import Ast (Expr(..), Stmt(..), Body, Type(..), IfMatchBody (IfMatchBody), LetBinding (..))
import Data.Text (Text, pack)
import TextShow (TextShow(showt))
import Error (Error(ICE, what, TypeError), Span (..))
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Text.Megaparsec (SourcePos(sourceColumn), unPos, mkPos)
import Data.Bifunctor (second, first)

-- hoistMaybe polyfill, because for some reason it's not in a reasonable verion of base 
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- Envirornments for variables (gamma) and types (delta)
data Env = Env { gamma :: M.HashMap Text (Bool, Type), delta :: M.HashMap Text Type }
    deriving Show
-- Infer monad
type Infer = StateT Env (Either Error)
-- Substitutions map variable indices to types
type Subst = M.HashMap Int Type

-- Error handling
instance MonadFail (Either Error) where fail m = Left $ ICE { what = pack m }
liftError :: (Text -> Infer a) -> Either Text a -> Infer a
liftError f (Left what) = f what
liftError _ (Right val) = lift $ Right val
throwError :: Span -> Text -> Either Error a
throwError s what = Left $ TypeError what s
throwErrorAt :: Expr -> Text -> Infer a
throwErrorAt = (lift .) . throwError . location

-- Substitution functions
unify :: Type -> Type -> Either (Type, Type) Subst
unify (Var nm i) b = return $ M.singleton i b
unify a (Var nm i) = return $ M.singleton i a
unify (Appl a a_ps) (Appl b b_ps) = do
    s1 <- unify a b
    s2 <- zipWithM unify a_ps b_ps
    return . mconcat $ s1 : s2
unify (Func a_ps a_r) (Func b_ps b_r) = unify (Appl a_r $ map snd a_ps) (Appl b_r $ map snd b_ps)
unify a b
    | a == b = Right M.empty
    | otherwise = Left (a, b)

compose :: Subst -> Subst -> Either Text Subst
compose a b
    | all_same_type a b = return $ a <> b
    -- Composing two substitutions with conflicting replacements for the same variable
    -- yields an error
    | otherwise = Left $
        "Conflicting types for type parameter " <> pack (show a) <> " and " <> pack (show b)
    where
        all_same_type :: Subst -> Subst -> Bool
        all_same_type = ((and . M.elems) .) . M.intersectionWith (==)
composeMany :: [Subst] -> Either Text Subst
composeMany = foldl (\a s -> a >>= compose s) (return M.empty)

applySubst :: Subst -> Type -> Type
applySubst s (Var nm i) = case M.lookup i s of
    Just ty -> ty
    Nothing -> Var nm i
applySubst s (Func params ret_type) = Func (map (second $ applySubst s) params) (applySubst s ret_type)
applySubst s (Appl base args) = Appl (applySubst s base) (map (applySubst s) args)
applySubst s t = t

-- has_type checks whether a typed expression's type is in the list
hasType :: Expr -> [Type] -> Infer Type
hasType expr tys = case find (== type_ expr) tys of
    Just ty -> return ty
    Nothing -> throwErrorAt expr $ "Could not match types : " <> showt (type_ expr) <> " and " <> showt tys

numeric :: [Type]
numeric = [Base "Real", Base "Int", Base "Nat", Base "Byte"]

inferStmts :: [Stmt] -> Infer [Stmt]
inferStmts = mapM inferStmt
inferStmt :: Stmt -> Infer Stmt
inferStmt stmt = case stmt of
    If bodies _ else_prog -> do
        typed_bodies <- mapM (withNewScope . infer_body) bodies
        typed_else_prog <- withNewScope $ runMaybeT $ do
            prog <- hoistMaybe else_prog
            lift $ inferStmts prog
        return stmt { bodies = typed_bodies, elseProg = typed_else_prog }
        where
            infer_body :: IfMatchBody -> Infer IfMatchBody
            infer_body (IfMatchBody cond loc body) = do
                typed_cond <- inferExpr cond
                typed_cond `hasType` [Base "Bool"]
                typed_body <- inferStmts body
                return $ IfMatchBody typed_cond loc typed_body
    Loop is_until cond body -> do
        typed_cond <- inferExpr cond
        typed_cond `hasType` [Base "Bool"]
        typed_body <- withNewScope $ inferStmts body
        return stmt {condU = typed_cond, bodyU = typed_body }
    Return expr -> Return <$> inferExpr expr
    Break -> return Break
    GroupDef name _ fields extends -> do
        env <- get
        traverse_ (\(gname, loc) -> maybeToInfer loc "Unknown type" $ M.lookup name (delta env)) extends
        put $ env { delta = M.insert name (Group name (fst <$> fields) (fst <$> extends)) (delta env) }
        return stmt
        where
            maybeToInfer :: Span -> Text -> Maybe a -> Infer a
            maybeToInfer _ _ (Just a) = lift $ Right a
            maybeToInfer e w Nothing  = lift $ throwError e w
    EnumDef name _ fields -> do
        env <- get ; put $ env { delta = M.insert name (Enum name fields) (delta env) }
        return stmt
    AliasDef name _ aliasee -> do
        env <- get ; put $ env { delta = M.insert name (Alias name aliasee) (delta env) }
        return stmt
    FuncDef name _ params ret_type _ body -> do
        env <- get
        let new_env = env { gamma = M.insert name (True, Func (zip (map (Just . fst) params) $ map (fst . snd) params) ret_type) (gamma env) }
            func_scope = new_env { gamma = M.union (M.fromList $ map (\(k, (v, s)) -> (k, (True, v))) params) (gamma new_env) }
        put func_scope ; typed_body <- mapM infer_func_stmt body ; put new_env
        return stmt { bodyFu = typed_body }
        where
            infer_func_stmt :: Stmt -> Infer Stmt
            infer_func_stmt (Return expr) = do
                typed_expr <- inferExpr expr
                typed_expr `hasType` [ret_type] -- TODO: Replace error
                return $ Return typed_expr
            infer_func_stmt stmt = inferStmt stmt
    For variable var_loc start end by body -> do
        typed_end <- inferExpr end
        end_type <- typed_end `hasType` numeric

        [typed_start, typed_by] <- mapM (\expr -> runMaybeT $ do
            expr <- hoistMaybe expr
            typed_expr <- lift $ inferExpr expr
            lift $ typed_expr `hasType` [end_type]
            return typed_expr) [start, by]

        env <- get ; case M.lookup variable (gamma env) of
            Just (_, ty) | ty /= end_type -> lift $ throwError var_loc "Could not match types"
            Nothing -> modify $ const env { gamma = M.insert variable (False, end_type) (gamma env) }
            _ -> pure ()
        typed_body <- withNewScope $ inferStmts body
        return stmt { startValue = typed_start, endValue = typed_end, byValue = typed_by, bodyFo = typed_body }
    Match variable blocks other -> do
        typed_variable <- inferExpr variable
        typed_blocks <- mapM (withNewScope . infer_block (type_ typed_variable)) blocks
        typed_otherwise <- runMaybeT $ do
            prog <- hoistMaybe other
            lift $ inferStmts prog
        return stmt { value = typed_variable, bodies = typed_blocks, other = typed_otherwise }
        where
            infer_block :: Type -> IfMatchBody -> Infer IfMatchBody
            infer_block variable_type (IfMatchBody value loc body) = do
                typed_value <- inferExpr value
                typed_value `hasType` [variable_type]
                typed_body <- inferStmts body
                return $ IfMatchBody typed_value loc typed_body
    Let is_const bindings -> do
        typed_bindings <- traverse inferBinding bindings
        return stmt { bindings = typed_bindings }
        where
            inferBinding :: LetBinding -> Infer LetBinding
            inferBinding l@(LetBinding name name_loc anon anon_loc val) = do
                typed_value <- case anon of
                    Just ty -> case val of
                        -- Empty list and dictionary literals' types cannot be inferred without some
                        -- (subset of) a bidirectional type checking algorithm, so we manually check
                        -- for list or dictionary types in a let statement
                        List _ _ [] ->
                            if isList ty then return val { type_ = ty }
                            else lift $ throwError (fromJust anon_loc) "Not a list type for empty list annotation"
                        Dict _ [] _ ->
                            if isDict ty then return val { type_ = ty }
                            else lift $ throwError (fromJust anon_loc) "Not a dict type for empty list annotation"
                        _ -> do
                            typed_value <- inferExpr val
                            if ty /= type_ typed_value
                                then throwErrorAt val "Mismatched types between annotation and value"
                                else return typed_value
                    Nothing -> inferExpr val

                -- This is not actually part of the type checking, but i haven't found a better place to put it yet
                env <- get ; case M.lookup name (gamma env) of
                    Just _ -> lift $ throwError name_loc  "Variable already declared"
                    Nothing -> modify $ const env { gamma = M.insert name (is_const, type_ typed_value) (gamma env) }

                return (l { value = typed_value } :: LetBinding)
            isList :: Type -> Bool
            isList (Appl (Base "List") _) = True
            isList _ = False
            isDict :: Type -> Bool
            isDict (Appl (Base "Map") _) = True
            isDict _ = False
    ExprStmt expr -> ExprStmt <$> inferExpr expr
    where
        withNewScope :: Infer a -> Infer a
        withNewScope comp = do
            oldScope <- get ; res <- comp
            put oldScope    ; return res

inferExpr :: Expr -> Infer Expr
inferExpr expr = case expr of
    Binary _ lhs rhs "." _ _ -> do
        typed_lhs <- inferExpr lhs
        case (type_ typed_lhs, rhs) of
            (Group gname fields _, Variable _ _ name) -> case M.lookup name fields of
                Just field -> let typed_ident = rhs { type_ = Base "Ident" }
                               in return $ expr { rhs = typed_ident, type_ = field }
                Nothing -> throwErrorAt rhs $ "Unknown field " <> name <> " in group " <> gname
            _ -> throwErrorAt lhs "The dot operator only operates on group types and identifiers"
    Binary _ lhs rhs "=" _ _ -> do
        env <- get ; typed_rhs <- inferExpr rhs
        -- Constrains the left-hand side of the assignment operator to variable names,
        -- array/dict indices and dot accessors
        case lhs of
            Variable _ _ name ->
                case M.lookup name (gamma env) of
                    Just (False, ty) -> if ty == type_ typed_rhs
                        then return $ expr { rhs = typed_rhs, type_ = ty }
                        else throwErrorAt rhs "Type mismatch in assingment"
                    Just (True, _) -> throwErrorAt lhs "Cannot assign to const variable"
                    Nothing -> throwErrorAt lhs "Assignment to undeclared variable"
                    -- TODO: Differentiate between error types
            Call _ callee args _ _ -> do
                verify_valid_callee callee
                typed_callee <- inferExpr callee
                typed_args <- mapM inferExpr args
                case type_ typed_callee of
                    -- Function calls also act as list/dict accessors
                    Appl (Base "List") [el_ty] -> do
                        when (el_ty /= type_ typed_rhs) $ throwErrorAt rhs $ "Expected " <> showt el_ty
                            <> " as rhs of assignment (now " <> showt (type_ typed_rhs) <> ")"
                        case map type_ typed_args of
                            [Base "Nat"] ->
                                let typed_call = lhs { callee = typed_callee, args = typed_args, type_ = el_ty }
                                 in return expr { lhs = typed_call, rhs = typed_rhs, type_ = el_ty }
                            _ -> throwErrorAt lhs "Wrong arguments to list index"
                    Appl (Base "Map") [key_ty, val_ty] -> do
                        when (val_ty == type_ typed_rhs) $ throwErrorAt rhs $
                            "Expected " <> showt val_ty <> "as rhs of assignment"
                        case map type_ typed_args of
                            [arg_ty] | arg_ty == key_ty ->
                                let typed_call = lhs { callee = typed_callee, args = typed_args, type_ = val_ty }
                                 in return expr { lhs = typed_call, rhs = typed_rhs, type_ = val_ty }
                            _ -> throwErrorAt lhs "Wrong arguments to map index"
                    _ -> throwErrorAt lhs $ "Expected a list index or map index as lhs of assignment " <> pack (show typed_callee)
            b@(Binary _ lhs var@(Variable _ _ name) "." _ _) -> do
                typed_lhs <- inferExpr lhs
                case type_ typed_lhs of
                    Group gname fields _ -> case M.lookup name fields of
                        Just ty -> return expr { lhs = b { lhs = typed_lhs }, rhs = typed_rhs, type_ = ty }
                        Nothing -> throwErrorAt var $ "Unknown field " <> name <> " in group " <> gname
                    _ -> throwErrorAt lhs "Expected a group"
            _ -> throwErrorAt lhs "Cannot assign"
        where
            unsnoc :: [a] -> Maybe ([a], a)
            unsnoc l | null l = Nothing
                        | otherwise = Just (init l, last l)
            verify_valid_callee :: Expr -> Infer ()
            verify_valid_callee (Call _ callee args _ _) = verify_valid_callee callee
            verify_valid_callee (Variable _ _ name) = do
                env <- get ; case M.lookup name (gamma env) of
                    Just (False, _) -> return ()
                    Just (True, _) -> throwErrorAt lhs "Cannot assign to const variable"
                    Nothing -> throwErrorAt lhs $ "Unknown variable " <> name
            verify_valid_callee _ = throwErrorAt lhs "Invalid lhs for array/map index assignment"
    Binary _ lhs rhs op _ _ -> do
        typed_lhs <- inferExpr lhs
        typed_rhs <- inferExpr rhs
        result_type <- case op of
            op | op `elem` [">", "<", "<=", ">="] -> do
                typed_lhs `hasType` numeric >> typed_rhs `hasType` numeric
                return $ Base "Bool"
            op | op `elem` ["==", "!="] -> do
                typed_lhs `hasType` [type_ typed_rhs]
                return $ Base "Bool"
            op | op `elem` ["and", "or"] -> do
                typed_lhs `hasType` [Base "Bool"] >> typed_rhs `hasType` [Base "Bool"]
                return $ Base "Bool"
            op | op `elem` ["+", "*", "**"] ->
                case mapM (\ty ->
                    if (ty == type_ typed_lhs && type_ typed_rhs `elem` numeric) ||
                    (ty == type_ typed_rhs && type_ typed_lhs `elem` numeric)
                    then Left ty
                    else Right ()) numeric of
                        Left ty -> return ty
                        Right _ -> throwErrorHere $
                            "Incompatible types for operator: " <> showt (type_ typed_lhs)
                            <> " and " <> showt (type_ typed_rhs)
            "/" -> do
                typed_lhs `hasType` numeric >> typed_rhs `hasType` numeric
                return $ Base "Real"
            "-" -> if type_ typed_lhs == Base "Nat" || type_ typed_rhs == Base "Nat"
                then return $ Base "Nat"
                else type_ <$> inferExpr expr { op = "+" }
            _ -> error "Unreachable"
        return $ expr { lhs = typed_lhs, rhs = typed_rhs, type_ = result_type }
    Unary _ rhs op _ _ -> do
        typed_rhs <- inferExpr rhs
        result_type <- case op of
            "not" -> typed_rhs `hasType` [Base "Bool"] >> return (Base "Bool")
            "~" -> typed_rhs `hasType` numeric >> return (type_ typed_rhs)
            "-" -> typed_rhs `hasType` numeric >> return
                (if type_ typed_rhs == Base "Nat" then Base "Int" else type_ typed_rhs)
            op -> throwErrorHere $ "Invalid unary operator " <> op
        return expr { rhs = typed_rhs, type_ = result_type }
    Number _ _ n -> return expr { type_ = Base "Nat" }
    String _ _ s -> return expr { type_ = Base "String" }
    Variable _ _ name -> do
        env <- get ; case M.lookup name (gamma env) of
            Just (_, ty) -> return expr { type_ = ty }
            Nothing -> throwErrorHere $ "Unknown variable " <> name
    Call _ callee args leftParen loc -> do
        typed_callee <- inferExpr callee
        typed_args <- mapM inferExpr args
        result_type <- case type_ typed_callee of
            Func params ret_type -> do
                assoc_list <- liftError (lift . throwError (Span (endLocation leftParen) ((endLocation loc) { sourceColumn = mkPos $ unPos (sourceColumn (endLocation loc)) - 1 })))
                    $ strict_zip params (map type_ typed_args) typed_args ([1..] :: [Int])
                substs <- mapM (\((name, ty), param, expr, i) -> case unify ty param of
                    Left (a, b) -> throwErrorAt expr $ case name of
                        Just name_ -> "Expected a " <> showt a <> " as argument to " <> name_ <> ", instead got " <> showt b
                        Nothing -> "Expected a " <> showt a <> " as argument " <> showt i <> ", instead got " <> showt b
                    Right hm -> lift $ Right hm) assoc_list
                composed <- liftError throwErrorHere $ composeMany substs
                return $ applySubst composed ret_type
            Appl (Base "List") [el_ty] -> case map type_ typed_args of
                [Base "Nat"] -> return el_ty
                _ -> throwErrorHere "Expected one Nat as array index"
            _ -> throwErrorAt callee "Cannot call a non-function variable"
        return expr { callee = typed_callee, args = typed_args, type_ = result_type }
        where
            strict_zip :: [a] -> [b] -> [c] -> [d] -> Either Text [(a, b, c, d)]
            strict_zip as bs cs ds
                | length as == length bs && length bs == length cs = return $ zip4 as bs cs ds
                | otherwise = Left $ "Expected " <> showt (length as) <> " arguments, but got " <> showt (length bs)
    List _ _ els -> do
        typed_els <- mapM inferExpr els
        case typed_els of
            [] -> throwErrorHere "Cannot determine type of empty list"
            _ -> do
                mapM_ (`hasType` [type_ $ head typed_els]) typed_els
                return expr { els = typed_els, type_ = Appl (Base "List") [type_ $ head typed_els] }
    Dict _ els _ -> do
        dict <- mapM (mapM inferExpr) $ transpose $ map (\(a, b) -> [a, b]) els
        case dict of
            [typed_keys, typed_values] -> do
                mapM_ (\(hd : tl) -> mapM (`hasType` [type_ hd]) tl) [typed_keys, typed_values]
                return expr { kvpairs = zip typed_keys typed_values, type_ = Appl (Base "Map") [type_ (head typed_keys), type_ (head typed_values)] }
            _ -> throwErrorHere "Cannot determine type of empty dictionary"
    Char _ _ c -> return expr { type_ = Base "Char" }
    Specialisation _ base args _ -> do
        typed_base <- inferExpr base
        let ftvs = sort $ nub $ ftv (type_ typed_base)
        subst <- case composeMany $ zipWith M.singleton ftvs args of
            Left txt -> throwErrorHere txt
            Right subst -> return subst
        let result_type = applySubst subst (type_ typed_base)
        return expr { base = typed_base, type_ = result_type }
        where
            ftv :: Type -> [Int]
            ftv (Var _ i) = [i]
            ftv (Appl base args) = ftv base ++ (args >>= ftv)
            ftv (Func args ret) = (args >>= ftv . snd) ++ ftv ret
            ftv (Alias _ t) = ftv t
            ftv (Group _ fields supers) = M.elems fields >>= ftv
            ftv _ = []
    where
        throwErrorHere :: Text -> Infer a
        throwErrorHere = lift . throwError (location expr)