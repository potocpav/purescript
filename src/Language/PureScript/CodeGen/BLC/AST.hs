module Language.PureScript.CodeGen.BLC.AST where

import Prelude.Compat

import Data.Text (Text)
import Data.Maybe (Maybe)

import Language.PureScript.AST (SourceSpan(..))

data AST
  = Abs Text AST
  | App AST AST
  | Var Text
  | If AST AST AST -- ^ If cond true_case false_case
  | StringEq AST AST
  | StringLiteral Text
  | NumericLiteral (Either Integer Double)
  | BooleanLiteral Bool
  | ArrayLiteral [AST]
  | Undefined
  | Unimpl Text

data BLC
  = BLC Text [Import] [Definition]

data Definition = Definition Text AST

data Import = Import Text
