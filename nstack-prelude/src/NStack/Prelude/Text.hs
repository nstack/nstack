{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NStack.Prelude.Text where

import Control.Monad (mzero)
import Data.Char (toUpper)
import Data.Serialize (Serialize(..), Putter, Get)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.Text.Lazy (toStrict)
import Text.PrettyPrint.Mainland (Doc, Pretty, ppr, prettyLazyText, stack)

-- | Join lines after inserting a newline in-between.
--
-- Note: this is not equivalent to 'unlines', as it does
-- not add a trailing newline.
joinLines :: [Text] -> Text
joinLines = intercalate "\n"

showT :: Show a => a -> Text
showT = pack . show

prettyT :: Int -> Doc -> Text
prettyT with = toStrict . prettyLazyText with

prettyT' :: Doc -> Text
prettyT' = prettyT 80

prettyLinesOr :: Pretty a => [a] -> Text -> Text
prettyLinesOr [] def = def
prettyLinesOr xs _   = prettyT' . stack . map ppr $ xs

pprT :: Pretty a => a -> Text
pprT = prettyT' . ppr

pprS :: Pretty a => a -> String
pprS = unpack . prettyT' . ppr

putText :: Putter Text
putText = put . encodeUtf8

getText :: Get Text
getText = either (const mzero) return . decodeUtf8' =<< get

putString :: Putter String
putString = putText . pack

getString :: Get String
getString = unpack <$> getText

-- | Capitalise the first char of a string
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : xs

capitaliseT :: Text -> Text
capitaliseT = pack . capitalise . unpack

