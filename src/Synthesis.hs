{-# LANGUAGE UndecidableInstances #-}

-- | Here's where we keep all the plumbing for actually generating an SMT 
--   problem. 
module Synthesis where

import API
import Types 
import Input

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Functor.Identity

import Data.Maybe

import Data.SBV hiding (name)
import qualified Data.SBV as SBV

import Data.Bits

import Control.Monad

import Control.Monad.State

import Control.Lens

-- The type of values as they exist within the SBV monad, we keep track of what
-- we named them as well as the actual ref
type SymbValue = Named SBV
type SymbPortData = PortData SymbValue
type SymbPort = Port SymbValue
type SymbElem = Elem SymbValue

-- The type of values as they exist outside the SBV monad, as the strings
-- needed to retrive them from one of SBV's models 
type RefValue = Name
type RefPortData = PortData RefValue
type RefPort = Port RefValue
type RefElem = Elem RefValue

-- The type of values as they exist outside the SBV monad, as the strings
-- needed to retrive them from one of SBV's models 
type OutValue = Named Identity
type OutPortData = PortData OutValue
type OutPort = Port OutValue
type OutElem = Elem OutValue

-- | Our model of the current problem, It captures all the relevant data in 
--   the system. The UID is just for convinience, it's only really useful when
--   we're actually building stuff out and assigning the UIDs but I don't 
--   actually really care.
data Model f = Model {
    getUIDCounter     :: UID
  , getLinks          :: Map LinkUID  (Elem f)
  , getBlocks         :: Map BlockUID (Elem f)
  , getLinkPorts      :: Map LinkPortUID  (Port f)
  , getBlockPorts     :: Map BlockPortUID (Port f)
  , getConnections    :: Map BlockPortUID (Map LinkPortUID  (f Bool))
  , getRevConnections :: Map LinkPortUID  (Map BlockPortUID (f Bool))
  }

makeLensesWith abbreviatedFields ''Model

-- This is where we need Undecidable instances. It's fine since we have an
-- acyclic dependency draph : Model -> Elem -> Port -> PortData
deriving instance (Show (Elem f), Show (Port f), Show (f Bool)) => Show (Model f)
deriving instance (Read (Elem f), Read (Port f), Read (f Bool)) => Read (Model f)

-- | The actual CSP assembly monad, it mostly keeps track of the momentary
--   internal state of the Symbolic Monad as we build things out. 
type Symb = StateT (Model (Named SBV)) Symbolic

-- | Grab a new UID and update the counter as needed.
newUID :: Symb UID
newUID = uIDCounter <+= 1

-- | Create the relevant symbolic variable and constraints for a named input
--   value.
symbValue :: forall a. SymWord a => NamedInputValue a -> Symb (Named SBV a)
symbValue (Named name Unused) = undefined
symbValue (Named name (Constraints cs)) = do
  sv <- lift $ free name
  let nv = Named name sv
  mapM_ (symbConstraint nv) cs
  return nv
  where
    -- | Add each individual constraint to the symbolic variable that already
    --   exists. 
    symbConstraint :: Named SBV b -> (String,Constraint b) -> Symb ()
    symbConstraint Named{..} (name,Is v)
      = lift . namedConstraint name $ getValue .== literal v
    symbConstraint Named{..} (name,OneOf vs)
      = lift . namedConstraint name $ sElem getValue (map literal vs)
    symbConstraint Named{..} (name,NoneOf vs)
      = lift . namedConstraint name $ bnot (sElem getValue (map literal vs))
    symbConstraint Named{..} (name,Between min max)
      = lift . namedConstraint name $ (literal min .<= getValue) &&& (getValue .<= literal max)
    symbConstraint Named{..} (name,SetFlags MaskedFlags{..})
      = lift . namedConstraint name $ (getValue .&. literal getMask) .== literal getFlags

-- | Naively convert portdata into its symbolic version
symbPortData :: NamedInputPortData -> Symb SymbPortData
symbPortData SW{..} = SW
  <$> symbValue getDirection
  <*> symbValue getApi
  <*> symbValue getApiFlags
  <*> symbValue getApiUID
  <*> symbValue getHostUID
  <*> symbValue getIsGPIO
symbPortData DigitalIO{..} = DigitalIO
  <$> symbValue getDirection
  <*> symbValue getZeroLevel
  <*> symbValue getOneLevel
  <*> symbValue getZeroThreshold
  <*> symbValue getOneThreshold
  <*> symbValue getApi
  <*> symbValue getApiFlags
  <*> symbValue getApiUID
symbPortData Power{..} = Power
  <$> symbValue getDirection
  <*> symbValue getVoltage
  <*> symbValue getCurrentDraw
  <*> symbValue getCurrentSupply

-- | Convert a port into its symbolic representation.
symbPort :: NamedInputPort -> Symb SymbPort
symbPort Port{..} = Port getName getRawUID
  <$> symbValue getUID
  <*> symbValue getUsed
  <*> symbValue getConnected
  <*> symbValue getConnectedUID
  <*> symbPortData getPortData

-- | Convert an Elem to its symbolid representation
symbElem :: NamedInputElem -> Symb SymbElem
symbElem Elem{..} = Elem getName getRawUID
  <$> symbValue getUID
  <*> symbValue getUsed
  <*> mapM symbPort getPorts

-- | Generic function to add an element to the design. Can specialize later
--   by using the correct pair of lenses.
addElem :: Lens' (Model (Named SBV)) (Map UID (Elem (Named SBV)))
        -> Lens' (Model (Named SBV)) (Map UID (Port (Named SBV)))
        -> InputElem -- the actual input
        -> (String -> SymbElem -> Symb ()) -- The closure for extra constraints
        -> Symb () -- The action that will generation the element
addElem elemLens portLens input constrain = do
  sed <- symbElem =<< nameElem "" <$> initElem newUID input
  -- NOTE :: Yes, I know partial functions are the devil. I should enforce it
  --         in the type somehow, but the assumption is that we have an
  --         assigned UID by this point. Our call to `initElem` should
  --         do the assignment. 
  elemLens %= Map.insert (fromJust $ sed ^. rawUID) sed
  -- Insert all the ports into the appropriate part of the model
  mapM_ (\ p -> portLens %= Map.insert (fromJust $ p ^. rawUID) p) $ sed ^. ports
  -- TODO :: Here is where we'd add all the general plumbing
  --   forall p in Ports. e.used == p.used
  constrain (sed ^. name) sed


-- | Add a block to the design, see `addElem` for a more detailed breakdown of
--   inputs and the like.
--
--   We also add the links neccesary for every block, which means that you
--   have to ensure that all the links are added before the blocks. 
--   I'd do it in a more robust way, but this is fine for now.
addBlock :: InputElem -> (String -> SymbElem -> Symb ())
         -> Symb ()
addBlock = addElem blocks blockPorts

-- | Add a link to the design, see `addElem` for a more detailed breakdown of
--   inputs and the like. 
addLink :: InputElem -> (String -> SymbElem -> Symb ())
         -> Symb ()
addLink = addElem links linkPorts

-- | Adds any edges that might be missing for any element in our design, 
--   making sure to add them to rhe 
addEdges :: Symb ()
addEdges = do
  bps <- uses blockPorts Map.keys
  lps <- uses linkPorts  Map.keys
  forM_ ((,) <$> bps <*> lps) (\ (bp,lp) -> do
    connExists <- isJust <$> uses connections (Map.lookup bp >=> Map.lookup lp)
    unless connExists . join $
      addEdge <$> uses blockPorts (fromJust . Map.lookup bp)
              <*> uses linkPorts  (fromJust . Map.lookup lp))
  where
    -- | Checks whether ports are of the same kind, adds the edge if so. 
    --   Assumes that ports are not already in either map.
    --
    --   NOTE :: For the type vsersion we're going to just stick all the 
    --           relevant constraints in the element definition, in practice
    --           we'd have an inheritace heirarchy of port types or something
    --           to make writing library definitions easier. 
    addEdge :: SymbPort -> SymbPort -> Symb ()
    addEdge a@Port{ getPortData = ad@SW{}} b@Port{ getPortData = bd@SW{}} = do
      connVar <- addEdgeHelper a b
      let cName = connVar^.name ++ " : Connected w/ Eq Types"
      -- `^?!` is the unsafe :: s -> Getting (Endo a) s a -> a 
      -- it works like `^."` but you have to enforce that the field actually 
      -- exists yourself. 
      lift . namedConstraint cName $ connVar^.value ==> bAnd [
        a^.portData.direction.value  .== b^.portData.direction.value,
        a^?!portData.api.value       .== b^?!portData.api.value,
        a^?!portData.apiFlags.value  .== b^?!portData.apiFlags.value,
        a^?!portData.apiUID.value    .== b^?!portData.apiUID.value,
        a^?!portData.hostUID.value   .== b^?!portData.hostUID.value,
        a^?!portData.isGPIO.value    .== b^?!portData.isGPIO.value]
    addEdge a@Port{ getPortData = ad@DigitalIO{}} b@Port{ getPortData = bd@DigitalIO{}} = do
      connVar <- addEdgeHelper a b
      let cName = connVar^.name ++ " : Connected w/ Eq Types"
      lift . namedConstraint cName $ connVar^.value ==> bAnd [
        a^.portData.direction.value      .== b^.portData.direction.value,
        a^?!portData.zeroLevel.value     .== b^?!portData.zeroLevel.value,
        a^?!portData.oneLevel.value      .== b^?!portData.oneLevel.value,
        a^?!portData.zeroThreshold.value .== b^?!portData.zeroThreshold.value,
        a^?!portData.oneThreshold.value  .== b^?!portData.oneThreshold.value,
        a^?!portData.api.value           .== b^?!portData.api.value,
        a^?!portData.apiFlags.value      .== b^?!portData.apiFlags.value,
        a^?!portData.apiUID.value        .== b^?!portData.apiUID.value]
    addEdge a@Port{ getPortData = ad@Power{}} b@Port{ getPortData = bd@Power{}} = do
      connVar <- addEdgeHelper a b
      let cName = connVar^.name ++ " : Connected w/ Eq Types"
      lift . namedConstraint cName $ connVar^.value ==> bAnd [
        a^.portData.direction.value      .== b^.portData.direction.value,
        a^?!portData.voltage.value       .== b^?!portData.voltage.value,
        a^?!portData.currentDraw.value   .== b^?!portData.currentDraw.value,
        a^?!portData.currentSupply.value .== b^?!portData.currentSupply.value]
    addEdge _ _ = return ()

    -- | Assumes that the edge isn't in either map already. Does not try to 
    --   add type equality checking.
    addEdgeHelper :: SymbPort -> SymbPort -> Symb (Named SBV Bool)
    addEdgeHelper bPort lPort = do
      let connName = bPort ^. name ++ " <-> " ++ lPort ^. name
          consNameT = connName ++ " : Connected"
      connVar <- lift $ free connName
      let ncv = Named connName connVar
          bpUID = fromJust $ bPort^.rawUID
          lpUID = fromJust $ lPort^.rawUID
          tf = connVar ==> bAnd [
                  -- If we're connected, both ports are being used and
                  -- connected.
                  bPort^.connected.value,bPort^.used.value,
                  lPort^.connected.value,lPort^.used.value,
                  -- If we're connected, both ports have the UID of the other.
                  bPort^.connectedUID.value .== literal lpUID,
                  lPort^.connectedUID.value .== literal bpUID]
      lift $ namedConstraint consNameT tf
      connections %= Map.unionWith Map.union
         (Map.singleton bpUID $ Map.singleton lpUID ncv)
      revConnections %= Map.unionWith Map.union
         (Map.singleton lpUID $ Map.singleton bpUID ncv)
      return ncv

-- ## Extract.hs ##
-- extractPortData
-- extractPort
-- extractElem
-- extractEdge
-- extractModel

-- ## Library.hs ##
-- <smattering of parts>

-- ## Specification.hs ##
-- genSpec
-- shrinkSpec

-- ## Problem ##
-- runProblem

-- ## AllSet ##
-- constrainTopology
-- allDesigns

-- ## Search ##
-- searchSpec

-- ## Main ## 
-- <various test cases>
