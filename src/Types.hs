{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import qualified Data.HashMap.Strict as M
import Data.List (intercalate, find, transpose, sort)
import Data.Either (rights, lefts, isLeft)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT), hoistMaybe) 
import Control.Monad.Trans (lift)
import Control.Monad.State (modify, StateT, MonadState (get, put))
import Control.Monad (forM)
import Ast (Expr(..), Stmt(..), Body, Type(..), ExprWith (getExpr, getInfo, ExprWith))
import Data.Text (Text, pack)
import TextShow (TextShow(showt))

data Env = Env { gamma :: M.HashMap Text (Bool, Type), delta :: M.HashMap Text Type }
type Infer = StateT Env (Either Text)
type Subst = M.HashMap Int Type

unify :: Type -> Type -> Infer Subst
unify (Var i) b = return $ M.singleton i b
unify a (Var i) = return $ M.singleton i a
unify (Appl a a_ps) (Appl b b_ps) = do
    s1 <- unify a b
    s2 <- mapM (uncurry unify) $ zip a_ps b_ps
    return . mconcat $ s1 : s2
unify (Func a_ps a_r) (Func b_ps b_r) = unify (Appl a_r a_ps) (Appl b_r b_ps)
unify a b
    | a == b || a `does_extend` b || b `does_extend` a = return M.empty
    | otherwise = throwError $ "Unification error: " <> showt a <> " with " <> showt b
    where
        does_extend :: Type -> Type -> Bool
        does_extend (Group _ supers) b@(Group _ _) = b `elem` supers
        does_extend _ _ = False

compose :: Subst -> Subst -> Infer Subst
compose a b
    | all_same_type a b = return $ a <> b
    | otherwise = throwError "Conflicting types for type parameter"
    where
        all_same_type :: Subst -> Subst -> Bool
        all_same_type = ((and . M.elems) .) . M.intersectionWith (==)
compose_many :: [Subst] -> Infer Subst
compose_many substs = foldl (\a s -> a >>= compose s) (return M.empty) substs

apply_subst :: Subst -> Type -> Type
apply_subst s (Var i) = case M.lookup i s of
    Just ty -> ty
    Nothing -> (Var i)
apply_subst s (Func params ret_type) = Func (map (apply_subst s) params) (apply_subst s ret_type)
apply_subst s (Appl base args) = Appl (apply_subst s base) (map (apply_subst s) args)
apply_subst s t = t

infer_stmts :: [Stmt ()] -> Infer [Stmt Type]
infer_stmts = mapM infer_stmt

has_type :: ExprWith Type -> [Type] -> Infer Type
has_type expr tys = case find (== getInfo expr) tys of
    Just ty -> return ty
    Nothing -> lift $ Left "Could not match types"

instance MonadFail (Either Text) where fail = Left . pack
throwError :: Text -> Infer a
throwError = lift . Left

numeric :: [Type]
numeric = [Base "Real", Base "Int", Base "Nat", Base "Byte"]
infer_stmt :: Stmt () -> Infer (Stmt Type)
infer_stmt (If bodies else_prog) = do
    typed_bodies <- mapM infer_body bodies
    typed_else_prog <- runMaybeT $ do
        prog <- hoistMaybe else_prog
        typed_prog <- lift $ infer_stmts prog
        return typed_prog
    return $ If typed_bodies typed_else_prog
    where
        infer_body :: (ExprWith (), Body ()) -> Infer (ExprWith Type, Body Type)
        infer_body (cond, body) = do
            typed_cond <- infer_expr cond
            typed_cond `has_type` [Base "Bool"] 
            typed_body <- infer_stmts body
            return (typed_cond, typed_body)
infer_stmt (Loop is_until cond body) = do
    typed_cond <- infer_expr cond
    typed_cond `has_type` [Base "Bool"] 
    typed_body <- infer_stmts body
    return $ Loop is_until typed_cond typed_body
infer_stmt (Return expr) = Return <$> infer_expr expr
infer_stmt Break = return Break
infer_stmt (GroupDef name fields extends) = do
    env <- get
    extends_types <- traverse (lift . maybeToRight "Unknown type" . flip M.lookup (delta env)) extends
    put $ env { delta = M.insert name (Group fields extends_types) (delta env) }
    return $ GroupDef name fields extends
    where
        maybeToRight :: b -> Maybe a -> Either b a 
        maybeToRight _ (Just a) = Right a
        maybeToRight e Nothing  = Left e
infer_stmt (EnumDef name fields) = do
    env <- get ; put $ env { delta = M.insert name (Enum fields) (delta env) }
    return $ EnumDef name fields
infer_stmt (AliasDef name aliasee) = do
    env <- get ; put $ env { delta = M.insert name (Alias name aliasee) (delta env) }
    return $ AliasDef name aliasee
infer_stmt (FuncDef name params ret_type body) = do
    env <- get
    let new_env = env { gamma = M.insert name (True, Func (M.elems params) ret_type) (gamma env) }
        func_scope = new_env { gamma = M.union (M.map (False,) params) (gamma new_env) }
    put func_scope ; typed_body <- mapM infer_func_stmt body ; put new_env
    return $ FuncDef name params ret_type typed_body
    where
        infer_func_stmt :: Stmt () -> Infer (Stmt Type)
        infer_func_stmt (Return expr) = do
            typed_expr <- infer_expr expr
            typed_expr `has_type` [ret_type]
            return $ Return typed_expr
        infer_func_stmt stmt = infer_stmt stmt
infer_stmt (For variable start end by body) = do
    typed_end <- infer_expr end
    end_type <- typed_end `has_type` numeric

    [typed_start, typed_by] <- mapM (\expr -> runMaybeT $ do
        expr <- hoistMaybe expr
        typed_expr <- lift $ infer_expr expr
        lift $ typed_expr `has_type` [end_type]
        return typed_expr) [start, by]

    env <- get ; case M.lookup variable (gamma env) of
        Just (_, ty) | ty /= end_type -> throwError "Could not match types"
        Nothing -> modify $ const env { gamma = M.insert variable (False, end_type) (gamma env) }
    typed_body <- infer_stmts body
    return $ For variable typed_start typed_end typed_by typed_body
infer_stmt (Match variable blocks otherwise) = do
    typed_variable <- infer_expr variable
    typed_blocks <- mapM (infer_block $ getInfo typed_variable) blocks
    typed_otherwise <- runMaybeT $ do
        prog <- hoistMaybe otherwise
        typed_prog <- lift $ infer_stmts prog
        return typed_prog
    return $ Match typed_variable typed_blocks typed_otherwise
    where
        infer_block :: Type -> (ExprWith (), Body ()) -> Infer (ExprWith Type, Body Type)
        infer_block variable_type (value, body) = do
            typed_value <- infer_expr value
            typed_value `has_type` [variable_type]
            typed_body <- infer_stmts body
            return (typed_value, typed_body)
infer_stmt (Let is_const name annotation value) = do
    typed_value <- case annotation of
        Just ty -> case getExpr value of
            List [] ->
                if isList ty then return ExprWith { getExpr = List [], getInfo = ty }
                else throwError "Not a list type for empty list annotation"
            Dict [] ->
                if isDict ty then return ExprWith { getExpr = Dict [], getInfo = ty }
                else throwError "Not a dict type for empty list annotation"
            _ -> do
                typed_value <- infer_expr value
                if ty /= getInfo typed_value
                    then throwError "Mismatched types between annotation and value"
                    else return typed_value
        Nothing -> infer_expr value

    env <- get ; case M.lookup name (gamma env) of
        Just _ -> throwError "Variable already declared"
        Nothing -> modify $ const env { gamma = M.insert name (is_const, getInfo typed_value) (gamma env) }
    return $ Let is_const name annotation typed_value
    where
        isList :: Type -> Bool
        isList (Appl (Base "List") _) = True 
        isList _ = False
        isDict :: Type -> Bool
        isDict (Appl (Base "Map") _) = True 
        isDict _ = False
infer_stmt (ExprStmt expr) = ExprStmt <$> infer_expr expr

ftv :: Type -> [Int]
ftv (Var i) = [i]
ftv (Appl base args) = ftv base ++ (args >>= ftv)
ftv (Func args ret) = (args >>= ftv) ++ ftv ret
ftv (Alias _ t) = ftv t
ftv (Group fields supers) = (M.elems fields >>= ftv) ++ (supers >>= ftv)
ftv _ = []

infer_expr :: ExprWith () -> Infer (ExprWith Type)
infer_expr expr = case getExpr expr of
    Binary lhs rhs "." -> do
        typed_lhs <- infer_expr lhs
        case (getInfo typed_lhs, getExpr rhs) of
            ((Group fields super_types), (Variable name)) -> case M.lookup name fields of
                Just field ->
                    let typed_ident = ExprWith { getExpr = Variable name, getInfo = Base "Ident" }
                     in return $ ExprWith { getExpr = Binary typed_lhs typed_ident ".", getInfo = field }
                Nothing -> throwError "Unknown fields in group"
            _ -> throwError "The dot operator only operates on group types and identifiers"
    Binary lhs rhs "=" -> do
        env <- get ; typed_rhs <- infer_expr rhs
        case getExpr lhs of
            Variable name ->
                case M.lookup name (gamma env) of
                    Just (False, ty) -> if ty == getInfo typed_rhs
                        then let typed_var = ExprWith { getExpr = Variable name, getInfo = ty}
                              in return $ ExprWith { getExpr = Binary typed_var typed_rhs "=", getInfo = ty }
                        else throwError "Type mismatch in assingment"
                    Just (True, _) -> throwError "Cannot assign to const variable"
                    Nothing -> throwError "Assignment to undeclared variable"
                    -- TODO: Differentiate between error types
            Call callee args -> do
                verify_valid_callee $ getExpr callee
                typed_callee <- infer_expr callee
                typed_args <- mapM infer_expr args
                case getInfo typed_callee of
                    (Appl (Base "List") [el_ty]) -> do
                        assert (el_ty == getInfo typed_rhs) $ "Expected " <> showt el_ty
                            <> " as rhs of assignment (now " <> (showt $ getInfo typed_rhs) <> ")"
                        case map getInfo typed_args of
                            [Base "Nat"] ->
                                let typed_call = ExprWith { getExpr = Call typed_callee typed_args, getInfo = el_ty }
                                 in return ExprWith { getExpr = Binary typed_call typed_rhs "=", getInfo = el_ty }
                            _ -> throwError "Wrong arguments to list index"
                    (Appl (Base "Map") [key_ty, val_ty]) -> do
                        assert (val_ty == getInfo typed_rhs) $ "Expected " <> showt val_ty <> "as rhs of assignment"
                        case map getInfo typed_args of
                            [arg_ty] | arg_ty == key_ty ->
                                let typed_call = ExprWith { getExpr = Call typed_callee typed_args, getInfo = val_ty }
                                 in return ExprWith { getExpr = Binary typed_call typed_rhs "=", getInfo = val_ty }
                            _ -> throwError "Wrong arguments to map index"
                    _ -> throwError $ "Expected a list index or map index as lhs of assignment"
            Binary lhs (ExprWith { getExpr = Variable name }) "." -> do
                typed_lhs <- infer_expr lhs
                case getInfo typed_lhs of
                    Group fields _ -> case M.lookup name fields of
                        Just ty ->
                            let typed_variable = ExprWith { getExpr = Variable name, getInfo = ty }
                             in return ExprWith { getExpr = Binary typed_lhs typed_variable ".", getInfo = ty }
                        Nothing -> throwError "Non-existant field in group"
                    _ -> throwError "Expected a group"
            _ -> throwError "Cannot assign"
        where
            assert :: Bool -> Text -> Infer ()
            assert True _ = return ()
            assert False s = throwError s
            unsnoc :: [a] -> Maybe ([a], a)
            unsnoc l | null l = Nothing
                     | otherwise = Just (init l, last l)
            verify_valid_callee :: Expr () -> Infer ()
            verify_valid_callee (Call callee args) = verify_valid_callee $ getExpr callee
            verify_valid_callee (Variable name) = do
                env <- get ; case M.lookup name (gamma env) of
                    Just (False, _) -> return ()
                    Just (True, _) -> throwError "Cannot assign to const variable"
                    Nothing -> throwError $ "Unknown variable " <> name
            verify_valid_callee _ = throwError "Invalid lhs for array/map index assignment"
    Binary lhs rhs op -> do
        typed_lhs <- infer_expr lhs
        typed_rhs <- infer_expr rhs
        result_type <- case op of
            op | op `elem` [">", "<", "<=", ">="] -> do
                typed_lhs `has_type` numeric >> typed_rhs `has_type` numeric
                return $ Base "Bool"
            op | op `elem` ["==", "!="] -> do
                typed_lhs `has_type` [getInfo typed_rhs]
                return $ Base "Bool"
            op | op `elem` ["and", "or"] -> do
                typed_lhs `has_type` [Base "Bool"] >> typed_rhs `has_type` [Base "Bool"]
                return $ Base "Bool"
            op | op `elem` ["+", "*", "**"] ->
                case mapM (\ty ->
                    if (ty == getInfo typed_lhs && getInfo typed_rhs `elem` numeric) ||
                    (ty == getInfo typed_rhs && getInfo typed_lhs `elem` numeric)
                    then Left ty
                    else Right ()) numeric of
                        Left ty -> return ty
                        Right _ -> throwError $
                            "Incompatible types for operator: " <> (showt $ getInfo typed_lhs)
                            <> " and " <> (showt $ getInfo typed_rhs)
            "/" -> do
                typed_lhs `has_type` numeric >> typed_rhs `has_type` numeric
                return $ Base "Real"
            "-" -> if getInfo typed_lhs == Base "Nat" || getInfo typed_rhs == Base "Nat"
                then return $ Base "Int"
                else getInfo <$> infer_expr ExprWith { getExpr = Binary lhs rhs "+", getInfo = () }
        return $ ExprWith { getExpr = Binary typed_lhs typed_rhs op, getInfo = result_type }
    Unary rhs op -> do
        typed_rhs <- infer_expr rhs
        result_type <- case op of
            "not" -> typed_rhs `has_type` [Base "Bool"] >> return (Base "Bool")
            "~" -> typed_rhs `has_type` numeric >> return (getInfo typed_rhs)
            "-" -> typed_rhs `has_type` numeric >> return
                (if getInfo typed_rhs == Base "Nat" then Base "Int" else getInfo typed_rhs)
            op -> throwError $ "Invalid unary operator " <> op
        return ExprWith { getExpr = Unary typed_rhs op, getInfo = result_type }
    Number n -> return ExprWith { getExpr = Number n, getInfo = Base "Nat" }
    String s -> return ExprWith { getExpr = String s, getInfo = Base "String" }
    Variable name -> do
        env <- get ; case M.lookup name (gamma env) of
            Just (_, ty) -> return ExprWith { getExpr = Variable name, getInfo = ty }
            Nothing -> throwError $ "Unknown variable " <> name
    Call callee args -> do
        typed_callee <- infer_expr callee
        typed_args <- mapM infer_expr args
        result_type <- case getInfo typed_callee of
            Func params ret_type -> do
                assoc_list <- lift $ strict_zip params $ map getInfo typed_args
                substs <- mapM (uncurry unify) assoc_list
                composed <- compose_many substs
                return $ apply_subst composed ret_type
            Appl (Base "List") [el_ty] -> case map getInfo typed_args of
                [Base "Nat"] -> return el_ty
                _ -> throwError "Expected one Nat as array index"
            _ -> throwError "Cannot call a non-function variable"
        return ExprWith { getExpr = Call typed_callee typed_args, getInfo = result_type }
        where
            strict_zip :: [a] -> [b] -> Either Text [(a, b)]
            strict_zip as bs
                | length as == length bs = return $ zip as bs
                | otherwise = Left "Mismatched lengths for strict_zip"
    List els -> do
        typed_els <- mapM infer_expr els
        case typed_els of
            [] -> throwError "Cannot determine type of empty list"
            _ -> do
                mapM (`has_type` [getInfo $ head typed_els]) typed_els
                return ExprWith { getExpr = List typed_els, getInfo = (Appl (Base "List") [getInfo $ head typed_els]) } 
    Dict els -> do
        dict <- mapM (mapM infer_expr) $ transpose $ map (\(a, b) -> [a, b]) els
        case dict of
            [typed_keys, typed_values] -> do
                mapM (\(hd : tl) -> mapM (`has_type` [getInfo hd]) tl) [typed_keys, typed_values]
                return ExprWith {getExpr = Dict (zip typed_keys typed_values), getInfo = Appl (Base "Map") [getInfo (head typed_keys), getInfo (head typed_values)] }
            _ -> throwError "Cannot determine type of empty dictionary"
    Specialisation base args -> do
        typed_base <- infer_expr base
        let ftvs = sort $ ftv (getInfo typed_base)
        subst <- compose_many $ map (uncurry M.singleton) $ zip ftvs args
        let result_type = apply_subst subst (getInfo typed_base)
        return ExprWith { getExpr = Specialisation typed_base args, getInfo = result_type }