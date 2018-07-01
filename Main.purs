module Main where

-- import Prelude

-- foreign import data Unit :: Type
-- foreign import unit :: Unit
-- foreign import log :: String -> Unit

main :: String -> String
main s = case s of
    "0" -> "0"
    _ -> "1"
