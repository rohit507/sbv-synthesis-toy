
module Extract where

import API
import Types 
import Input
import Synthesis

import Data.Map (Map)
import qualified Data.Map as Map

import Data.SBV hiding (name)
import qualified Data.SBV as SBV

import Control.Monad.Reader

import Control.Lens

-- The type of values as they exist outside the SBV monad, as the strings
-- needed to retrive them from one of SBV's models 
type RefValue = Name
type RefPortData = PortData RefValue
type RefPort = Port RefValue
type RefElem = Elem RefValue
type RefModel = Model RefValue

-- The type of values as they exist outside the SBV monad, as the strings
-- needed to retrive them from one of SBV's models 
type OutValue = Named Identity
type OutPortData = PortData OutValue
type OutPort = Port OutValue
type OutElem = Elem OutValue
type OutModel = Model OutValue

-- | Given a portdata block with names and something else, get one with 
--   only names. 
stripPortData :: PortData (Named a) -> RefPortData
stripPortData SW{..} = SW {
    getDirection = toName getDirection,
    getApi       = toName getApi,
    getApiFlags  = toName getApiFlags,
    getApiUID    = toName getApiUID,
    getHostUID   = toName getHostUID,
    getIsGPIO    = toName getIsGPIO
  }
stripPortData DigitalIO{..} = DigitalIO {
    getDirection = toName getDirection,
    getZeroLevel = toName getZeroLevel,
    getOneLevel = toName getOneLevel,
    getZeroThreshold = toName getZeroThreshold,
    getOneThreshold = toName getOneThreshold,
    getApi = toName getApi,
    getApiFlags = toName getApiFlags,
    getApiUID = toName getApiUID
  }
stripPortData Power{..} = Power {
    getDirection = toName getDirection,
    getVoltage = toName getVoltage,
    getCurrentDraw = toName getCurrentDraw,
    getCurrentSupply = toName getCurrentSupply
  }

-- | Strip the non-name metadata from a port
stripPort :: Port (Named a) -> RefPort
stripPort Port{..} = Port {
    getName = getName,
    getRawUID = getRawUID,
    getUID = toName getUID,
    getUsed = toName getUsed,
    getConnected = toName getConnected,
    getConnectedUID = toName getConnectedUID,
    getPortData = stripPortData getPortData
  }

-- | Strip the non-name metadata from an element
stripElem :: Elem (Named a) -> RefElem
stripElem Elem{..} = Elem{
    getName = getName,
    getRawUID = getRawUID,
    getUID = toName getUID,
    getUsed = toName getUsed,
    getPorts = fmap stripPort getPorts
  }

-- | Strip the non-name metadata from a model
stripModel :: Model (Named a) -> RefModel
stripModel Model{..} = Model{
    getUIDCounter = getUIDCounter,
    getLinks = fmap stripElem getLinks,
    getBlocks = fmap stripElem getBlocks,
    getLinkPorts = fmap stripPort getLinkPorts,
    getBlockPorts = fmap stripPort getBlockPorts,
    getConnections = fmap (fmap toName) getConnections,
    getRevConnections = fmap (fmap toName) getRevConnections
  }


-- ## Extract.hs ##
-- extractPortData
-- extractPort
-- extractElem
-- extractEdge
-- extractModel
