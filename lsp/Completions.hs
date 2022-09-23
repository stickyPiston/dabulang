{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Completions where

import Data.Text (Text, unpack, pack)
import Language.LSP.Types (Position (Position), CompletionItem (..), List (..))
import Text.Megaparsec (parse, SourcePos (SourcePos), unPos, mkPos)
import Parser (programP)
import Ast hiding (List)
import Error (Span(..))
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Types
import Std (env)
import Control.Monad.Trans.State (evalStateT)

data CompletionVariable = CompletionVariable { pos :: SourcePos, name :: Text, type_ :: Type }
data CompletionScope = CompletionScope { position :: Span, vars :: [CompletionVariable], scopes :: [CompletionScope] }
emptyScope :: Span -> CompletionScope
emptyScope span = CompletionScope { position = span, vars = [], scopes = [] }

findCompletions :: CompletionScope -> Position -> List CompletionItem
findCompletions scope pos =
    let sourcePos = posToSourcePos pos
        availableVars = filter (\(CompletionVariable pos _ _) -> pos < sourcePos) (vars scope)
        rec = mconcat $ mapMaybe (\scope@CompletionScope { position = (Span begin end) } ->
            if posToSourcePos pos < end then Just $ findCompletions scope pos else Nothing) $ scopes scope
     in rec <> List (map createCompletionItem availableVars)
    where
        posToSourcePos :: Position -> SourcePos
        posToSourcePos (Position row col) = (SourcePos "" `on` mkPos . fromIntegral) row col
        createCompletionItem :: CompletionVariable -> CompletionItem
        createCompletionItem CompletionVariable { pos, Completions.name, Completions.type_ } =
            CompletionItem
            { _label = name, _kind = Nothing
            , _tags = Nothing, _detail = Just $ "As " <> pack (show type_)
            , _documentation = Nothing, _deprecated = Nothing
            , _preselect = Nothing, _sortText = Nothing
            , _filterText = Nothing, _insertText = Nothing
            , _insertTextFormat = Nothing, _textEdit = Nothing
            , _additionalTextEdits = Nothing, _commitCharacters = Nothing
            , _command = Nothing, _xdata = Nothing, _insertTextMode = Nothing
            }

gatherCompletions :: Text -> CompletionScope
gatherCompletions source =
    let createSourcePos = SourcePos "" `on` mkPos
        zeroPosition = createSourcePos 0 0
     in case parse programP "" source of
        Right ast -> case evalStateT (mapM inferStmt ast) env of
            Right typed_ast ->
                let span = Span zeroPosition (findEndLocation $ unpack source)
                in foldl findDefintions (emptyScope span) typed_ast
            Left _ -> emptyScope (Span zeroPosition zeroPosition)
        Left err -> emptyScope (Span zeroPosition zeroPosition)
    where
        countIf, countWhile :: (a -> Bool) -> [a] -> Int
        countIf = (length .) . filter
        countWhile = go 0
            where go n p (x : xs) | p x = go (n + 1) p xs
                                  | otherwise = n
                  go n p [] = n
        findEndLocation :: String -> SourcePos
        findEndLocation s =
            let row = countIf (== '\n') s
                col = countWhile (/= '\n') $ reverse s
             in (SourcePos "" `on` mkPos) row col
        completionsFromLetBinding :: LetBinding -> CompletionVariable
        completionsFromLetBinding (LetBinding name (Span begin _) _ _ value) = CompletionVariable begin name $ Ast.type_ value 
        findDefintions :: CompletionScope -> Stmt -> CompletionScope
        findDefintions scope (Let _ bindings) = scope { vars = vars scope ++ map completionsFromLetBinding bindings }
        findDefintions scope (FuncDef name loc _ ty _ _ body span) =
            let filledScope = foldl findDefintions (emptyScope span) body
             in scope { vars = CompletionVariable (startLocation loc) name ty : vars scope, scopes = filledScope : scopes scope }
        findDefintions scope (If bodies _ else_ loc) =
            let ifScopes = map (\b -> foldl findDefintions (emptyScope $ bodySpan b) $ body b) bodies
                elseScope = case (else_, loc) of
                    (Just else_, Just loc) -> [foldl findDefintions (emptyScope loc) else_]
                    _ -> []
             in scope { scopes = elseScope ++ ifScopes ++ scopes scope }
        findDefintions scope (Loop _ _ body span) =
            scope { scopes = foldl findDefintions (emptyScope span) body : scopes scope }
        findDefintions scope (For var (Span varLoc _) _ _ _ body span) =
            let newScope = (emptyScope span) { vars = [CompletionVariable varLoc var None] }
             in scope { scopes = foldl findDefintions newScope body : scopes scope }
        findDefintions scope (Match _ bodies other span) =
            let bodyScopes = map (\(IfMatchBody _ _ body loc) -> foldl findDefintions (emptyScope loc) body) bodies
                otherScope = case (other, span) of
                    (Just body, Just span) -> [foldl findDefintions (emptyScope span) body]
                    _ -> []
             in scope { scopes = otherScope ++ bodyScopes ++ scopes scope }
        findDefintions scope _ = scope