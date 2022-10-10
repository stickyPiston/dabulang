{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval where

import Control.Monad.State (StateT, MonadState (..), modify, gets)
import qualified Data.HashMap.Strict as M
import Data.Text (Text, unpack, pack)
import Ast (Expr (..), Stmt (..), IfMatchBody(..), Body, Type (..), intercalateBuilder, LetBinding (..), Value (..), Eval, RuntimeError (..), extractNumber, variables, EvalState (..))
import Data.List (find, findIndex, intercalate)
import Control.Monad (forM_, forM, when, void)
import Control.Monad.Except (ExceptT, lift, runExceptT, liftIO, withExceptT)
import Control.Monad.Trans.Except (catchE, throwE)
import Data.Function (on)
import Data.Bifunctor (second, Bifunctor (bimap))
import Text.Read (readMaybe)
import Data.Bits (Bits(..))
import Error (Error (..), Span)
import TextShow (TextShow(showt, showb), fromString)

isTrue :: Value -> Bool
isTrue (VBool True) = True
isTrue _ = False

evalExpr :: Expr -> Eval Value
evalExpr expr = case expr of
    Number _ _ n -> return $ VNat n
    String _ _ txt -> return $ VString txt
    Variable _ loc name -> do
        env <- gets variables ; case env M.!? name of
            Nothing -> throwE $ Error $ LocatedRuntimeError ("Undefined variable " <> name) loc
            Just va -> return va
    Binary _ lhs (Variable _ _ field) "." _ _ -> do
        VGroup name fields <- evalExpr lhs
        return $ fields M.! field
    Binary _ lhs rhs "=" _ _ -> evalExpr rhs >>= assignAt lhs
        where
            assignAt :: Expr -> Value -> Eval Value
            assignAt (Variable _ _ name) val = modify (\state -> state { variables = M.insert name val (variables state) }) >> return val
            assignAt (Call _ callee [arg] _ loc) val = do
                lhs <- evalExpr callee
                arg <- evalExpr arg
                case type_ callee of
                    Appl (Base "List") _ -> case lhs of
                        VList vs ->
                            let newList = VList <$> replaceAtIndex loc vs val (extractNumber arg)
                             in newList >>= assignAt callee
                        _ -> throwICE "Variable marked having type List[T] is not a list"
                    -- Appl (Base "Map") _ -> case lhs of
                    --     VMap kvpairs -> case findIndex ((== arg) . fst) kvpairs of
                    --         Just index ->
                    --             let (key, _) = kvpairs !! index
                    --              in assignAt callee $ VMap $ replaceAtIndex kvpairs (key, val) index
                    --         Nothing -> error "" -- TODO: Runtime error
                    --     _ -> error "" -- should be unreachable
                    _ -> error "" -- should be unreachable
            assignAt (Binary _ lhs (Variable _ _ name) "." _ _) val = do
                group@(VGroup _ fields) <- evalExpr lhs
                assignAt lhs group { groupValues = M.insert name val fields }
            assignAt _ _ = error ""
            replaceAtIndex :: Span -> [Value] -> Value -> Int -> Eval [Value]
            replaceAtIndex _ [] val 0 = return [val]
            replaceAtIndex _ (x : xs) val 0 = return $ val : xs
            replaceAtIndex loc [] x n = throwE $ Error $ LocatedRuntimeError "Array index out of bounds" loc
            replaceAtIndex loc (x : xs) val n = (x :) <$> replaceAtIndex loc xs val (n - 1)
    Binary _ lhs rhs op _ loc -> do
        lhs <- evalExpr lhs
        rhs <- evalExpr rhs
        (case op of
            "+" -> VInt `typed` ((+) `on` extractNumber)
            "-" -> VInt `typed` ((-) `on` extractNumber)
            "/" -> \l r ->
                let li = extractNumber l
                    ri = extractNumber r
                 in if ri == 0
                        then throwE $ Error $ LocatedRuntimeError "Division by 0" loc
                        else return $ VInt $ li `div` ri
            "*" -> VInt `typed` ((*) `on` extractNumber)
            "and" -> VBool `typed` ((&&) `on` extractBool)
            "or" -> VBool `typed` ((||) `on` extractBool)
            "<<" -> VInt `typed` (shiftL `on` extractNumber)
            ">>" -> VInt `typed` (shiftR `on` extractNumber)
            "&" -> VInt `typed` ((.&.) `on` extractNumber)
            "|" -> VInt `typed` ((.|.) `on` extractNumber)
            "^" -> VInt `typed` (xor `on` extractNumber)
            "==" -> VBool `typed` (==)
            "!=" -> VBool `typed` (/=)
            "<" -> VBool `typed` ((<) `on` extractNumber)
            ">" -> VBool `typed` ((>) `on` extractNumber)
            "<=" -> VBool `typed` ((<=) `on` extractNumber)
            ">=" -> VBool `typed` ((>=) `on` extractNumber)
            _ -> \_ _ -> throwICE $ "Unknown binary operator " <> op) lhs rhs
        where
            typed :: (a -> Value) -> (Value -> Value -> a) -> Value -> Value -> Eval Value
            typed l r a b = return $ l $ r a b
    Unary _ rhs op _ _ -> do
        rhs <- evalExpr rhs
        let operation = case op of
                "~" -> return . VInt . complement . extractNumber
                "not" -> return . VBool . not . extractBool
                "-" -> return . VInt . negate . extractNumber
                _ -> const $ throwICE ("Unknown unary operator " <> op)
         in operation rhs
    Call _ callee uargs _ loc -> do
        fn <- evalExpr callee
        args <- mapM evalExpr uargs
        case fn of
            VFunc names body -> do
                state@(EvalState oldScope _) <- get
                lift $ put state { variables = oldScope <> M.fromList (zip names args) }
                returned <- catchE (Left <$> mapM_ evalStmt body) (return . Right)
                lift $ put state { variables = oldScope }
                case returned of
                    Left va -> return VSole
                    Right value -> case value of
                        err@(Error _) -> throwE err
                        ReturnValue value -> return value
                        BreakLoop -> throwE $ Error $ UnlocatedRuntimeError "Break out of function"
            VIntrin intrin -> intrin args
            VList els -> arrayIndex els args
            VConstructor f -> return $ f args
            _ -> error $ show callee
        where
            atMay :: [a] -> Int -> Maybe a
            atMay (x : xs) 0 = Just x
            atMay [] n = Nothing
            atMay (x : xs) n = atMay xs (n - 1)
            arrayIndex :: [Value] -> [Value] -> Eval Value
            arrayIndex els args = 
                let index = extractNumber (head args)
                 in case els `atMay` index of
                    Just el -> return el
                    Nothing -> throwE $ Error $ LocatedRuntimeError
                        ("Index out of bounds, requested index " <> showt index <>
                            ", but list only has " <> showt (length els) <> " elements")
                        loc
    List _ _ els -> VList <$> traverse evalExpr els
    Dict _ els _ -> VMap <$> (zip <$> traverse (evalExpr . fst) els <*> traverse (evalExpr . snd) els)
    Char _ _ c -> return $ VChar c
    Specialisation _ ex _ _ -> evalExpr ex
    where
        throwICE :: Text -> Eval a
        throwICE = throwE . Error . ICE

evalStmt :: Stmt -> Eval ()
evalStmt stmt = case stmt of
    If bodies _ else_ _ -> do
        conds <- zip (map body bodies) <$> forM bodies (evalExpr . cond)
        case find (isTrue . snd) conds of
          Nothing -> forM_ else_ (mapM_ evalStmt)
          Just (body, _) -> mapM_ evalStmt body
    Loop b cond sts _ -> evalBody b cond sts
        where
            evalBody :: Bool -> Expr -> Body -> Eval ()
            evalBody isUntil cond body = do
                t <- (if isUntil then not . isTrue else isTrue) <$> evalExpr cond
                when t $ do
                    a <- catchE (Right <$> mapM_ evalStmt body) (return . Left)
                    case a of
                        Left BreakLoop -> return ()
                        Left e -> throwE e
                        Right () -> evalBody b cond body
    Return ex -> evalExpr ex >>= throwE . ReturnValue
    Break -> throwE BreakLoop
    GroupDef name _ fields_ extends -> do
        delta <- gets types
        additionalParamNames <- getAllConstructorFields (fst <$> extends)
        let extensionNames = map fst extends
            fieldNames = fst <$> fields_
            construct = VConstructor $ \args -> VGroup name $ M.fromList $ zip (fieldNames ++ additionalParamNames) args 
         in modifyVariables $ M.insert name construct
         where
            getAllConstructorFields :: [Text] -> Eval [Text]
            getAllConstructorFields names = do
                delta <- gets types
                return $ concat [map fst fields | extension <- names, Group name fields extends <- [delta M.! extension]]
    EnumDef txt sp txts -> undefined
    AliasDef txt sp ty -> undefined
    FuncDef name _ params _ _ _ body _ -> modifyVariables $ M.insert name (VFunc (map fst params) body)
    For var _ start end by body _ -> do
        start <- evalMaybe start
        maybe (pure ()) (modifyVariables . M.insert var) start
        by <- evalMaybe by
        end <- evalExpr end
        evalBody var end (by `orElse` VNat 1) body
        where
            evalMaybe :: Maybe Expr -> Eval (Maybe Value)
            evalMaybe = traverse evalExpr
            evalBody :: Text -> Value -> Value -> Body -> Eval ()
            evalBody var end by body = do
                env <- gets variables
                when (((<) `on` extractNumber) (env M.! var) end) $ do
                    a <- catchE (Right <$> mapM_ evalStmt body) (return . Left)
                    case a of
                        Left BreakLoop -> return ()
                        Left e -> throwE e
                        Right () -> do
                            let incrementWith = ((+) `on` extractNumber) (env M.! var)
                             in modifyVariables $ M.insert var (VNat $ incrementWith by)
                            evalBody var end by body
            orElse :: Maybe a -> a -> a
            orElse (Just a) _ = a
            orElse Nothing a = a
    Match cond bodies other _ -> do
        variable <- evalExpr cond
        evalBlocks bodies other variable
        where
            evalBlocks :: [IfMatchBody] -> Maybe Body -> Value -> Eval ()
            evalBlocks ((IfMatchBody cond _ body _) : bodies) other variable = do
                cond <- evalExpr cond
                if cond == variable then mapM_ evalStmt body else evalBlocks bodies other variable
            evalBlocks [] Nothing _ = return ()
            evalBlocks [] (Just body) _ = mapM_ evalStmt body
    Let b bindings ->
        forM_ bindings $ \(LetBinding name _ _ _ expr) -> do
            value <- evalExpr expr
            modifyVariables $ M.insert name value
    ExprStmt ex -> void $ evalExpr ex

evalProgram :: Body -> ExceptT Error (StateT EvalState IO) ()
evalProgram prog = withExceptT transformError $ mapM_ evalStmt prog 
    where transformError :: RuntimeError -> Error
          transformError (Error er) = er
          transformError (ReturnValue va) = ICE "Unhandled return"
          transformError BreakLoop = ICE "Unhandled break"

modifyVariables :: (M.HashMap Text Value -> M.HashMap Text Value) -> Eval ()
modifyVariables f = modify $ \state -> state { variables = f $ variables state }