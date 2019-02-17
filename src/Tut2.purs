module Tut2 where

import Prelude

import Tut1 (Box(..))
import Data.String (Replacement(..), Pattern(..), replace)
import Control.Comonad (extract)


foreign import unsafeParseFloat :: String -> Number

moneyToFloat :: String -> Box Number
moneyToFloat str =
    Box str #
    map (replace (Pattern "$") (Replacement "")) #
    map (\replaced -> unsafeParseFloat replaced)

percentToFloat :: String -> Box Number
percentToFloat str =
    Box (replace (Pattern "%") (Replacement "") str) #
    map (\replaced -> unsafeParseFloat replaced) #
    map (_ * 0.01)    


applyDiscount :: String -> String -> Number
applyDiscount price discount =
    (extract $ moneyToFloat price) #
    (\cost -> (extract $ percentToFloat discount) #
        (\savings -> cost - cost * savings))    

applyDiscount' :: String -> String -> Number
applyDiscount' price discount = extract $
    (moneyToFloat price) >>=
        (\cost -> (percentToFloat discount) >>=
            (\savings -> pure $ cost - cost * savings))     

-- applyDiscount'' :: String -> String -> Number
-- applyDiscount'' price discount = do
--     cost <- moneyToFloat price
--     savings <- percentToFloat discount
--     pure $ cost - cost * savings              