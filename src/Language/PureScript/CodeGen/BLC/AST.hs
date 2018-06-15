module Language.PureScript.CodeGen.BLC.AST where

import Prelude.Compat

import Data.Text (Text)
import Data.Maybe (Maybe)

import Language.PureScript.AST (SourceSpan(..))

data AST
  = Abs Text AST
  | App AST AST
  | Var Text
  | StringLiteral Text
  | NumericLiteral (Either Integer Double)
  | BooleanLiteral Bool
  | ArrayLiteral [AST]
  | Unimpl Text

data BLC
  = BLC Text [Import] [Definition]

data Definition = Definition Text AST

data Import = Import Text
