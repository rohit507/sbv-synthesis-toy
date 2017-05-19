
module Extract where

import API
import Types 
import Input
import Synthesis

import Data.Map (Map)
import qualified Data.Map as Map

import Data.SBV hiding (name, extractModel)
import qualified Data.SBV as SBV

import Control.Monad.Reader
import Control.Monad.Except

import Control.Lens

-- The type of values as they exist outside the SBV monad, as the strings
-- needed to retrive them from one of SBV's models 
type RefValue = Name
type RefPortData = PortData RefValue
type RefPort     = Port     RefValue
type RefElem     = Elem     RefValue
type RefModel    = Model    RefValue

-- The type of values as they exist outside the SBV monad, as the strings
-- needed to retrive them from one of SBV's models 
type OutValue = Named Identity
type OutPortData = PortData OutValue
type OutPort     = Port     OutValue
type OutElem     = Elem     OutValue
type OutModel    = Model    OutValue

-- | The type of the monad in which we reconstruct values.
type Extract m = ReaderT m (Either String)

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
    getDirection     = toName getDirection,
    getZeroLevel     = toName getZeroLevel,
    getOneLevel      = toName getOneLevel,
    getZeroThreshold = toName getZeroThreshold,
    getOneThreshold  = toName getOneThreshold,
    getApi           = toName getApi,
    getApiFlags      = toName getApiFlags,
    getApiUID        = toName getApiUID
  }
stripPortData Power{..} = Power {
    getDirection     = toName getDirection,
    getVoltage       = toName getVoltage,
    getCurrentDraw   = toName getCurrentDraw,
    getCurrentSupply = toName getCurrentSupply
  }

-- | Strip the non-name metadata from a port
stripPort :: Port (Named a) -> RefPort
stripPort Port{..} = Port {
    getName         = getName,
    getRawUID       = getRawUID,
    getUID          = toName getUID,
    getUsed         = toName getUsed,
    getConnected    = toName getConnected,
    getConnectedUID = toName getConnectedUID,
    getPortData     = stripPortData getPortData
  }

-- | Strip the non-name metadata from an element
stripElem :: Elem (Named a) -> RefElem
stripElem Elem{..} = Elem{
    getName   = getName,
    getRawUID = getRawUID,
    getUID    = toName getUID,
    getUsed   = toName getUsed,
    getPorts  = fmap stripPort getPorts
  }

-- | Strip the non-name metadata from a model
stripModel :: Model (Named a) -> RefModel
stripModel Model{..} = Model{
    getUIDCounter     = getUIDCounter,
    getLinks          = fmap stripElem getLinks,
    getBlocks         = fmap stripElem getBlocks,
    getLinkPorts      = fmap stripPort getLinkPorts,
    getBlockPorts     = fmap stripPort getBlockPorts,
    getConnections    = fmap (fmap toName) getConnections,
    getRevConnections = fmap (fmap toName) getRevConnections
  }

-- | Convert a maybe into an error in the appropriate monad
justErr :: MonadError e m => e -> Maybe a -> m a
justErr _ (Just a) = return a
justErr e Nothing = throwError e

-- | Extract a value with a given name from the SMT output
extractValue :: (Modelable m, SymWord a) => Name a -> Extract m (OutValue a)
extractValue (Name s) = do
  val <- join $ justErr ("Failed to extract : " ++ s) . getModelValue s <$> ask
  return $ Named s (Identity val)

-- | Extract a particular portdata from the SMT Output
extractPortData :: (Modelable m) => RefPortData -> Extract m OutPortData
extractPortData SW{..} = SW
  <$> extractValue getDirection
  <*> extractValue getApi
  <*> extractValue getApiFlags
  <*> extractValue getApiUID
  <*> extractValue getHostUID
  <*> extractValue getIsGPIO
extractPortData DigitalIO{..} = DigitalIO
  <$> extractValue getDirection
  <*> extractValue getZeroLevel
  <*> extractValue getOneLevel
  <*> extractValue getZeroThreshold
  <*> extractValue getOneThreshold
  <*> extractValue getApi
  <*> extractValue getApiFlags
  <*> extractValue getApiUID
extractPortData Power{..} = Power
  <$> extractValue getDirection
  <*> extractValue getVoltage
  <*> extractValue getCurrentDraw
  <*> extractValue getCurrentSupply

-- | Extract a port from the SMT Output
extractPort :: Modelable m => RefPort -> Extract m OutPort
extractPort Port{..} = Port getName getRawUID
  <$> extractValue getUID
  <*> extractValue getUsed
  <*> extractValue getConnected
  <*> extractValue getConnectedUID
  <*> extractPortData getPortData

-- | extract an element from the SMT output
extractElem :: Modelable m => RefElem -> Extract m OutElem
extractElem Elem{..} = Elem getName getRawUID
  <$> extractValue getUID
  <*> extractValue getUsed
  <*> mapM extractPort getPorts

-- | Extract the data for an entire model from the SMT Output
extractModel :: Modelable m => RefModel -> Extract m OutModel
extractModel Model{..} = Model getUIDCounter
  <$> mapM extractElem getLinks
  <*> mapM extractElem getBlocks
  <*> mapM extractPort getLinkPorts
  <*> mapM extractPort getBlockPorts
  <*> mapM (mapM extractValue) getConnections
  <*> mapM (mapM extractValue) getRevConnections

-- | Wrapper type for a model that should keep us from having to
--   constantly recalculate the dictionary
--
--   TODO :: There has got to be a better way to do this :/ the standard 
--           interface just has the dictionary being recreated every time I want
--           get a value from it.
data ModelableWrapper a = MW{
    model  :: a,
    dict   :: Map String SBV.CW,
    modVal :: forall b. SymWord b => String -> Maybe b
  }

instance Modelable a => Modelable (ModelableWrapper a) where
  modelExists                  = modelExists . model
  getModel                     = getModel . model
  getModelDictionary           = dict
  getModelValue s a            = modVal a s
  getModelUninterpretedValue s = getModelUninterpretedValue s . model
  getModelObjectives           = getModelObjectives . model
  extractUnsatCore             = extractUnsatCore . model
  extractModel                 = SBV.extractModel . model

-- | Wrap a modelable to cache the dictionary
wrapModel :: Modelable a => a -> ModelableWrapper a
wrapModel a
  = MW{ model = a , dict = dc , modVal = \ s -> fromCW <$> Map.lookup s dc}
  where
    dc = getModelDictionary a


