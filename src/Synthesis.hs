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
import Data.Foldable

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
type SymbPort     = Port     SymbValue
type SymbElem     = Elem     SymbValue
type SymbModel    = Model    SymbValue

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

-- | Convert a computation in Symb to one in Symbolic that returns the 
--   model that we built up. 
runSymb :: Symb a -> Symbolic (a, SymbModel)
runSymb = flip runStateT Model{
    getUIDCounter = 0,
    getLinks = mempty,
    getBlocks = mempty,
    getLinkPorts = mempty,
    getBlockPorts = mempty,
    getConnections = mempty,
    getRevConnections = mempty
  }

-- | Grab a new UID and update the counter as needed.
newUID :: Symb UID
newUID = uIDCounter <+= 1

-- | Create the relevant symbolic variable and constraints for a named input
--   value.
symbValue :: forall a. SymWord a => NamedInputValue a -> Symb (SymbValue a)
symbValue (Named name Unused) = undefined
symbValue (Named name (Constraints cs)) = do
  nv <- Named name <$> lift (free name)
  mapM_ (symbConstraint nv) cs
  return nv
  where
    -- | Add each individual constraint to the symbolic variable that already
    --   exists. 
    symbConstraint :: SymbValue b -> (String,Constraint b) -> Symb ()
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
addElem :: Lens' SymbModel (Map UID SymbElem)
        -> Lens' SymbModel (Map UID SymbPort)
        -> InputElem -- the actual input
        -> (SymbElem -> Symb ()) -- The closure for extra constraints
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
  -- Add the constraint that ensures all the ports and the elem itself have
  -- the same used state. (i.e. `forall p in ports. elem.used == p.used`) 
  lift . namedConstraint (sed ^. name ++ " : Used Propagation") . bAnd $
    map (\ p -> sed^.used.value .== p^.used.value) (Map.elems $ sed ^. ports)
  -- Add the constraints given by the user
  constrain sed


-- | Add a block to the design, see `addElem` for a more detailed breakdown of
--   inputs and the like.
--
--   We also add the links neccesary for every block, which means that you
--   have to ensure that all the links are added before the blocks. 
--   I'd do it in a more robust way, but this is fine for now.
addBlock :: InputElem -> (SymbElem -> Symb ()) -> Symb ()
addBlock = addElem blocks blockPorts

-- | Add a link to the design, see `addElem` for a more detailed breakdown of
--   inputs and the like. 
addLink :: InputElem -> (SymbElem -> Symb ()) -> Symb ()
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
        fieldEq a b direction,
        fieldEq a b api,
        fieldEq a b apiFlags,
        fieldEq a b apiUID,
        fieldEq a b hostUID,
        fieldEq a b isGPIO]
    addEdge a@Port{ getPortData = ad@DigitalIO{}} b@Port{ getPortData = bd@DigitalIO{}} = do
      connVar <- addEdgeHelper a b
      let cName = connVar^.name ++ " : Connected w/ Eq Types"
      lift . namedConstraint cName $ connVar^.value ==> bAnd [
        fieldEq a b direction,
        fieldEq a b zeroLevel,
        fieldEq a b oneLevel,
        fieldEq a b zeroThreshold,
        fieldEq a b oneThreshold,
        fieldEq a b api,
        fieldEq a b apiFlags,
        fieldEq a b apiUID]
    addEdge a@Port{ getPortData = ad@Power{}} b@Port{ getPortData = bd@Power{}} = do
      connVar <- addEdgeHelper a b
      let cName = connVar^.name ++ " : Connected w/ Eq Types"
      lift . namedConstraint cName $ connVar^.value ==> bAnd [
        fieldEq a b direction,
        fieldEq a b voltage,
        fieldEq a b currentDraw,
        fieldEq a b currentSupply]
    -- We don't need to create an edge if their PortData constructors don't
    -- match, since those ports simply cannot be connected. 
    addEdge _ _ = return ()

    -- | constructs the term for checking whether two fields in the PortData
    --   are equal given the two ports and the lens to the field. 
    fieldEq :: SymbPort -> SymbPort -> Fold SymbPortData (SymbValue a) -> SBV Bool
    fieldEq a b lens = a^?!portData.lens.value .== b^?!portData.lens.value

    -- | Assumes that the edge isn't in either map already. Does not try to 
    --   add type equality checking.
    addEdgeHelper :: SymbPort -> SymbPort -> Symb (SymbValue Bool)
    addEdgeHelper bPort lPort = do
      let connectionName = bPort ^. name ++ " <-> " ++ lPort ^. name
          constraintName = connectionName ++ " : Connected"
      connVar <- lift $ free connectionName
      let ncv = Named connectionName connVar
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
      lift $ namedConstraint constraintName tf
      connections %= Map.unionWith Map.union
         (Map.singleton bpUID $ Map.singleton lpUID ncv)
      revConnections %= Map.unionWith Map.union
         (Map.singleton lpUID $ Map.singleton bpUID ncv)
      return ncv


-- | Constraints like ensuring that ports can only be connected to other
--   ports require global information, so we add them im last.
--
--   This should only be called once, after every other element of the 
--   synthesis process is complete. 
addFinalConstraints :: Symb ()
addFinalConstraints = do
  instrumentPorts connections    blockPorts
  instrumentPorts revConnections linkPorts

  where
    -- | Take all the ports in one connection map and add the constraint
    --   that ensures only one connection exists at a time. 
    instrumentPorts :: Lens' SymbModel (Map UID (Map UID (SymbValue Bool)))
                    -> Lens' SymbModel (Map UID SymbPort)
                    -> Symb ()
    instrumentPorts connLens portLens =
      traverse_ portConstraint' =<< attachSymbPort portLens =<< use connLens

    -- Just lookup every UID in the map and add the actual port information
    attachSymbPort :: Lens' SymbModel (Map UID SymbPort) -> Map UID a
                   -> Symb (Map UID (SymbPort,a))
    attachSymbPort lens = Map.traverseWithKey
      (\ uid a -> (,a) . fromJust <$> uses lens (Map.lookup uid))

    -- | Given a set of connections and their boolean flag variables, generate
    --   the constraint that only one is connected.
    onlyOneConnected :: Map UID (SymbValue Bool) -> SBV Bool
    onlyOneConnected = flip pbExactly 1 . Map.elems . fmap (^.value)

    -- | With a portData and a set of connections generate the connected 
    --   and unconnected constraints.
    portConstraint :: SymbPort -> Map UID (SymbValue Bool) -> Symb ()
    portConstraint port conns = lift $ namedConstraint consName constraint
      where
        consName = port^.name ++ " : Connected UID Consistency"
        constraint = (port^.connected.value ==> onlyOneConnected conns)
           &&& (bnot (port^.connected.value) ==>
                    (port^.connectedUID.value .== literal (-1)))

    -- | uncurried version of `portConstraint` so that I don't go 
    --   over the 80 char limit in instrumentPorts
    portConstraint' :: (SymbPort, Map UID (SymbValue Bool)) -> Symb ()
    portConstraint' = uncurry portConstraint

