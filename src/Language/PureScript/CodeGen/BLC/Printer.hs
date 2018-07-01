module Language.PureScript.CodeGen.BLC.Printer
  ( prettyPrintBLC
  ) where

import Prelude.Compat


import Data.Word (Word8)
import Data.Bits (testBit)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (unpack)
import Data.Monoid ((<>))
import Language.PureScript.CodeGen.BLC.AST (AST(..), BLC(..), Definition(..), Import(..))
import Data.Text (Text)
import qualified Data.Text as T

prettyPrintBLC :: BLC -> Text
prettyPrintBLC (BLC mn incls defs) = "\n" `T.intercalate` [imps, defs'] where
  imps = T.concat (printImport <$> incls)
  defs' = T.concat $ (flip T.append "\n" . printDefinition mn) <$> defs

printDefinition :: Text -> Definition -> Text
printDefinition mn (Definition nm def) = T.intercalate " " ["#define", mn <> "__" <> nm, printAST def]

parens :: Text -> Text
parens t = T.concat ["(", t, ")"]

printAST :: AST -> Text
printAST (Abs var ast) = parens $ T.concat ["\\", var, " ", printAST ast]
printAST (App a b) = parens $ T.intercalate " " [printAST a, printAST b]
printAST (Var v) = v
printAST (If cond t f) = parens $ T.intercalate " " [printAST cond, printAST t, printAST f]
printAST (StringEq s1 s2) = parens $ T.intercalate " " ["Prim__StringEq", printAST s1, printAST s2]
printAST (StringLiteral v) = printText v
printAST (NumericLiteral v) = parens $ T.concat ["NumericLiteral ", T.pack $ show v]
printAST (BooleanLiteral v) = if v then "Prim__T" else "Prim__F"
printAST (ArrayLiteral vs) = printArray printAST vs
printAST (Unimpl text) = text
printAST Undefined = "(\\a a)"

printImport :: Import -> Text
printImport (Import t) = "#include <" <> t <> "/index.lam>\n"

printArray :: (a -> Text) -> [a] -> Text
printArray f (a:as) = parens $ "Prim__C " <> f a <> " " <> printArray f as
printArray _ [] = "Prim__nil"

printText :: Text -> Text
printText t = printArray id $ printByte <$> unpack (encodeUtf8 t)

printByte :: Word8 -> Text
printByte byte = parens $ "Prim__B " <> T.intercalate " " (map (\b -> if b then "Prim__b1" else "Prim__b0") arr) where
  arr = testBit byte <$> [7,6..0]
