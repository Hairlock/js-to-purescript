module Tut4 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (catchException, error, message, throwException)
import Effect.Random (randomInt)

type PortRange = { min :: Int, max :: Int }

validPorts :: PortRange
validPorts = { min: 2500,  max: 7500 }

isInvalidPort :: Int -> Boolean
isInvalidPort portNumber =
  (portNumber < validPorts.min || portNumber > validPorts.max)

throwWhenBadPort ::  Int -> Effect Unit
throwWhenBadPort portNumber =
  when (isInvalidPort portNumber) $ throwException errorMessage
  where
    errorMessage = error $ "Error: expected a port number between " <>
                              show validPorts.min <> " and " <> show validPorts.max


catchWhenBadPort :: Int -> Effect Unit
catchWhenBadPort portNumber =
  catchException printException $ throwWhenBadPort portNumber
  where
    printException e = log $ message e

run :: Effect Unit
run = do
  log "Use chain for composable error handling with nested Eithers - Part 1"
  -- Create a 50% chance of generating a invalid port number
  portNumber <- randomInt (validPorts.min - 2500) (validPorts.max + 2500)
  log $ "Our random port number is: " <> show portNumber

  -- Try commenting out catchWhenBadPort and uncommenting throwWhenBadPort
  -- to see throwException in action
  catchWhenBadPort portNumber
--   throwWhenBadPort portNumber