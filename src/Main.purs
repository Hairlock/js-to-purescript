module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Tut1 (nextCharForNumberString)
import Tut2 (applyDiscount, applyDiscount')


main :: Effect Unit
main = do
  log $ nextCharForNumberString "    64   "
  logShow $ applyDiscount "$5.00" "20%"
  logShow $ applyDiscount' "$5.00" "20%"
