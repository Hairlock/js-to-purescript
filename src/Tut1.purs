module Tut1
    (Box(..), nextCharForNumberString) where

import Prelude

import Control.Comonad (class Comonad, class Extend, extract)
import Data.Char (fromCharCode)
import Data.Int (fromString)
import Data.Maybe (fromMaybe) 
import Data.String (toLower, trim)
import Data.String.CodeUnits (singleton)

newtype Box a = Box a

instance functorBox :: Functor Box where
  map f (Box x) = Box (f x)


instance showBox :: Show a => Show (Box a) where
  show (Box a) = "Box(" <> show a <> ")"

instance extendBox :: Extend Box where
  extend f m = Box (f m)

instance comonadBox :: Comonad Box where
  extract (Box x) = x

instance applyBox :: Apply Box where
  apply (Box f) (Box x) = Box (f x)

-- module.exports = { Box, of: Box }
instance applicativeBox :: Applicative Box where
  pure = Box

instance bindBox :: Bind Box where
  bind (Box m) f = f m
instance monadBox :: Monad Box  

nextCharForNumberString :: String -> String
nextCharForNumberString str =
  Box str #
  map trim #
  map (\s -> fromMaybe 0 $ fromString s) #
  map (\i -> i + 1) #
  map (\i -> fromMaybe ' ' $ fromCharCode i) #
  map (\c -> toLower $ singleton c) #
  extract

nextCharForNumberString' :: String -> String
nextCharForNumberString' =
    trim >>>
    fromString >>>
    fromMaybe 0 >>>
    (+) 1 >>>
    fromCharCode >>>
    fromMaybe ' ' >>>
    singleton