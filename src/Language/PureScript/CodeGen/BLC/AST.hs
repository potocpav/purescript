module Language.PureScript.CodeGen.BLC.AST where

import Data.Text (Text)
import Data.Maybe (Maybe)

import Language.PureScript.AST (SourceSpan(..))

data AST
  = Abs Text AST
  | App AST AST
  | Var Text
  | Unimpl Text

data Definition = Definition Text AST
