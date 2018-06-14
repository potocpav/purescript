module Main where

foreign import data Unit :: Type
-- foreign import unit :: Unit
foreign import log :: String -> Unit

main :: Unit
main = log "Done"
