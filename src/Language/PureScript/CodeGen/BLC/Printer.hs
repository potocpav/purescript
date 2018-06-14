module Language.PureScript.CodeGen.BLC.Printer
  ( prettyPrintBLC
  ) where

import Prelude.Compat

import Language.PureScript.CodeGen.BLC.AST (AST(..), Definition(..))
import Data.Text (Text, intercalate)
import qualified Data.Text as T

prettyPrintBLC :: [Definition] -> Text
prettyPrintBLC = intercalate "\n\n" . map printDefinition

printDefinition :: Definition -> Text
printDefinition (Definition nm def) = intercalate " " ["#define", nm, parens $ printAST def]

parens :: Text -> Text
parens t = T.concat ["(", t, ")"]

printAST :: AST -> Text
printAST (Abs var ast) = T.concat ["(\\", var, " ", printAST ast, ") "]
printAST (App a b) = intercalate " " ["App", printAST a, printAST b]
printAST (Var v) = T.concat ["Var ", v]
printAST (Unimpl text) = text
