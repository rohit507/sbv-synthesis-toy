
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


emptyLink :: InputElem
emptyLink = Elem{
    getName = undefined,
    getRawUID = Nothing,
    getUID = undefined,
    getUsed = undefined,
    getPorts = Map.empty
  }

emptyPort :: InputPort
emptyPort = Port{
    getName = "defaultPort",
    getRawUID = Nothing,
    getUID = undefined,
    getUsed = undefined,
    getConnected = undefined,
    getConnectedUID = undefined,
    getPortData = undefined
  }


-- * Links

-- PowerLink
powerLink = addLink emptyLink{
    getName = "powerLink",
    getPorts = [
      ("vio1",emptyPort{
          getPortData = Power{
            
          }
        }),
      ("vio2",undefined)
  } $ do
    return ()
    
-- DigitalIOLink 
-- SWLink

-- * Blocks 

-- PowerSupply
-- LED
-- MCU
-- LED Driver

-- * Specification block
