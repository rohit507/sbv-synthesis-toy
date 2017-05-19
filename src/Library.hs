
module Library where

import API
import Types 
import Input
import Synthesis

import Data.Map (Map)
import qualified Data.Map as Map

import Data.SBV hiding (name, extractModel)
import qualified Data.SBV as SBV

import Control.Monad.State

import Control.Lens



-- Shorthand for when we know the exact value of some input
is :: SymWord a => a -> InputValue a
is a = unnamed [Is a]

-- Shorthand for collecting a set of flags that must be on
flags :: (Enum a, Bounded a) => [a] -> InputValue FlagSet
flags = unnamed . (:[]) . SetFlags . toFlags . map (,True)

-- Shorthand for having no requirements at all
unknown :: SymWord a => InputValue a
unknown = unnamed []

-- | This lets us use record puns to let us define a bunch of context 
--   dependent helpers in a single place. 
data ElemUtil = ElemUtil {

    -- | The prefix that constraint names should be using
    prefix :: String,

    -- | Generate a constraint predicated on the element being used
    constraint :: String -> SBV Bool -> Symb (),

    -- | Get the data block for a particular port. 
    port :: String -> SymbPort,

    -- | Given two ports and lenses to identical values in them, this 
    --   tells you whether they're equal. 
    portValEq :: PortValEqType 
  }

-- | Constructor function for our helpers. 
mkElemUtil :: SymbElem -> ElemUtil
mkElemUtil e = ElemUtil{..}
  where
    prefix :: String
    prefix = e^.name ++ " : "

    constraint :: String -> SBV Bool -> Symb ()
    constraint s c = lift $ namedConstraint (prefix ++ s) (e^.used.value ==> c)

    port :: String -> SymbPort
    port n = e^?!ports.ix n

    portValEq :: PortValEqType
    portValEq a b l = port a ^?! l . value .== port b ^?! l . value

-- | Giant type we need to write out for some reason. 
type PortValEqType = forall a. (SymWord a)
        => String
        -> String
        -> (forall f. (Contravariant f, Applicative f)
              => (SymbValue a -> f (SymbValue a))
              -> SymbPort
              -> f SymbPort)
        -> SBV Bool

-- | A helper function for constructing elements
mkElem :: String -> [(String,InputPort)] -> InputElem
mkElem name ports = Elem {
  getName = name,
  getRawUID = undefined,
  getUID = undefined,
  getUsed = undefined,
  getPorts = Map.fromList ports
 }

-- | A helper function for constructing ports  
mkPort :: String -> InputPortData -> InputPort
mkPort name portData = Port {
  getName = name,
  getRawUID = undefined,
  getUID = undefined,
  getUsed = undefined,
  getConnected = undefined,
  getConnectedUID = undefined,
  getPortData = portData
 }
-- * Power

-- power
-- powerIn 
-- powerOut

-- powerLink

-- * DigitalIO

-- digital 
-- digitalI
-- digitalO
-- digitalBidir 

-- digitalLink 

-- * API 

testSpec :: Symb ()
testSpec = addBlock elem constraints
  where

    elem = mkElem "specification" [
          ("req", mkPort "testRequirement" SW{
                getDirection = is Sink,
                getApi = is LED,
                getApiFlags = flags [LRed,LBright],
                getApiUID = unknown,
                getHostUID = unknown,
                getIsGPIO = is False})]

    constraints e = do

      let ElemUtil{..} = mkElemUtil e

      -- We have to do this the long way since this component must be used 
      -- and the implicit predicate we ass in the helper function would 
      -- break that. 
      lift $ namedConstraint (prefix ++ "Block must exist") (e^.used.value)

      constraint "Req must be connected" $ port "req"^?!connected.value

swLink :: Symb ()
swLink = addLink elem constraints

  where

    elem = mkElem" swLink" [
          ("sink", mkPort "testRequirement" SW{
                getDirection = is Sink,
                getApi = unknown,
                getApiFlags = unknown,
                getApiUID = unknown,
                getHostUID = unknown,
                getIsGPIO = unknown}),
          ("source", mkPort "testRequirement" SW{
                getDirection = is Source,
                getApi = unknown,
                getApiFlags = unknown,
                getApiUID = unknown,
                getHostUID = unknown,
                getIsGPIO = unknown})]

    constraints e = do
      -- Using record puns to instantiate a bunch of shared helper functions
      -- that take a bit of extra context. 
      let ElemUtil{..} = mkElemUtil e

      -- We want the state of both connections to match, so that either 
      -- none are connected or both are connected. 
      constraint "Both Connections Expected" $
        portValEq "sink" "source" connected

      -- We need to set these various internal variables equal here so that
      -- the values get propagated through the link. 
      constraint "Port Data Must be Similar" $ bAnd [
        portValEq "sink" "source" (portData.api),
        portValEq "sink" "source" (portData.apiFlags),
        portValEq "sink" "source" (portData.apiUID),
        portValEq "sink" "source" (portData.hostUID),
        portValEq "sink" "source" (portData.isGPIO)]

testSource :: Symb ()
testSource = addBlock elem constraints

  where

    elem = mkElem "ledSource" [
          ("req",mkPort "testRequirement" SW {
                getDirection = is Source,
                getApi = is LED,
                getApiFlags = flags [LRed,LBright],
                getApiUID = unknown,
                getHostUID = unknown,
                getIsGPIO = is False})]

    -- This is just a placeholder block for testing, there's no need for
    -- any actual constraints. It'll take care of itself. 
    constraints e = return ()
