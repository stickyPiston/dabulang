{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parser
import Types
import Text.Megaparsec
import qualified Data.HashMap.Strict as M
import Control.Monad.Trans.State
import Std
import Error
import Language.LSP.Diagnostics
import Language.LSP.VFS hiding (_line, _character)
import System.IO hiding (liftIO)
import Data.List.NonEmpty (toList)

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler STextDocumentDidChange $ \msg -> do
        let NotificationMessage _ _ (DidChangeTextDocumentParams document _) = msg
            VersionedTextDocumentIdentifier uri version = document
            normalizedUri = toNormalizedUri uri
            virtualFile = getVirtualFile normalizedUri
            mcontent = maybe "" virtualFileText <$> virtualFile 
         in mcontent >>= \content -> sendDiagnostics content normalizedUri
  , notificationHandler STextDocumentDidOpen $ \msg ->
        let NotificationMessage _ _ (DidOpenTextDocumentParams document) = msg
            TextDocumentItem uri _ version content = document
         in sendDiagnostics content (toNormalizedUri uri)
  , notificationHandler STextDocumentDidSave $ \msg -> do
        let NotificationMessage _ _ (DidSaveTextDocumentParams document mcontent) = msg
            uri = _uri (document :: TextDocumentIdentifier)
            normalizedUri = toNormalizedUri uri
            content = maybe "" id mcontent
         in sendDiagnostics content normalizedUri
  , notificationHandler SInitialized $ \message -> return ()
  ]
  where
    createDiagnostic :: Show b => String -> ParseError T.Text b -> Diagnostic
    createDiagnostic source err = 
      let pos = getPosition source (errorOffset err) (Position 0 0)
        in Diagnostic (Range pos pos) (Just DsError) Nothing (Just "dabulanglsp") (T.pack $ show err) Nothing Nothing
    getPosition :: String -> Int -> Position -> Position
    getPosition [] _ pos = pos
    getPosition _ 0 pos = pos
    getPosition ('\n' : xs) offset pos = getPosition xs (offset - 1) pos { _line = _line pos + 1}
    getPosition (x : xs) offset pos = getPosition xs (offset - 1) pos { _character = _character pos + 1}
    sendDiagnostics :: T.Text -> NormalizedUri -> LspM () ()
    sendDiagnostics content uri = do
      let diagnostics = case parse programP (show uri) content of
            Right ast -> case evalStateT (mapM inferStmt ast) env of
                Right _ -> []
                Left (TypeError w s) -> [Diagnostic (spanToPosition s) (Just DsError) Nothing (Just "dabulanglsp") w Nothing Nothing]
                _ -> []
            Left err ->
                let errs = bundleErrors err
                  in map (createDiagnostic $ T.unpack content) $ toList errs
      flushDiagnosticsBySource 1 (Just "dabulanglsp")
      publishDiagnostics 1 uri Nothing (partitionBySource diagnostics)

spanToPosition :: Span -> Range
spanToPosition (Span begin end) = Range (Position (extract (sourceLine begin) - 1) (extract (sourceColumn begin) - 1)) (Position (extract (sourceLine end) - 1) (extract (sourceColumn end) - 1))
  where extract = fromIntegral . unPos

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ const $ Right ()
  , defaultConfig = ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions {
    textDocumentSync =
        Just
          ( TextDocumentSyncOptions
              (Just True)
              (Just TdSyncFull)
              (Just False)
              (Just False)
              (Just $ InR $ SaveOptions $ Just True)
          )
    }
  }