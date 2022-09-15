{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Error where

import Prelude hiding (length)
import Data.Text (Text, unpack, pack, length)
import Text.Megaparsec (SourcePos (..), unPos, ParseErrorBundle, errorBundlePretty)
import Ast (Span(..))
import Data.Void (Void)
import qualified Data.Text.IO as Text
import TextShow ( TextShow(showt) )

data Error
    = TypeError { what :: Text, location :: Span }
    | ICE { what :: Text }
    | LocatedRuntimeError { what :: Text, location :: Span }
    | UnlocatedRuntimeError { what :: Text }
    | ParseError { bundle :: ParseErrorBundle Text Void }
    deriving Show

printError :: Text -> Error -> Text
printError source ICE { what } = "Internal Compiler Error : " <> what
printError source TypeError { what, location = Span { startLocation, endLocation } } =
    "Program Error : " <> what <> "\n" <>
        if sourceLine startLocation == sourceLine endLocation
            then
                let prefix = showt (unPos $ sourceLine startLocation) <> " | "
                 in prefix <> pack (getLine (unpack source) (unPos $ sourceLine startLocation)) <> "\n"
                    <> pack (replicate (unPos (sourceColumn startLocation) - 1 + length prefix) ' ')
                    <> pack (replicate (unPos (sourceColumn endLocation) - unPos (sourceColumn startLocation)) '~')
            else undefined
    where
        getLine :: String -> Int -> String
        getLine source 1 = takeWhile (/= '\n') source
        getLine source lineNo = getLine (tail $ dropWhile (/= '\n') source) (lineNo - 1)
printError source ParseError { bundle } = pack $ errorBundlePretty bundle
printError source UnlocatedRuntimeError { what } = "Runtime Error : " <> what
printError source LocatedRuntimeError { what, location } = printError source $ TypeError what location