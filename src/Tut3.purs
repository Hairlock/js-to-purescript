module Tut3 where

import Prelude

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.List (List(..), dropWhile, head, null, (:))
import Data.Maybe (fromJust)
import Data.String (toUpper)
import Partial.Unsafe (unsafePartial)

foreign import sliceImpl :: Fn3 Int Int String String

slice :: Int -> Int -> String -> String
slice begin end string =
    runFn3 sliceImpl begin end string

type ColorName = String
type HexValue = String
type Error = Unit    

data Color = Color ColorName HexValue
type Colors = List Color

masterColors :: Colors
masterColors = (Color "red" "#ff4444")  :
               (Color "blue" "#44ff44") :
               (Color "yellow" "#fff68f") : Nil

fromList :: forall a. List a -> Either Unit a
fromList xs =
    if (null xs)
        then Left unit
        else Right $ unsafePartial fromJust $ head xs

findColor :: ColorName -> Either Error Color 
findColor colorName =
    fromList $ dropWhile (\(Color n _) -> n /= colorName) masterColors        

hex :: Color -> HexValue
hex (Color n h) = h

result :: ColorName -> String
result name =
    findColor name #
    map hex #
    map (slice 1 0) #
    either (\e -> "No color") toUpper