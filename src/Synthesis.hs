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

import Data.SBV

import Data.Bits

import Control.Monad.State

import Control.Lens

-- The type of values as they exist within the SBV monad, we keep track of what
-- we named them as well as the actual ref
type SymbValue = Named SBV
type SymbPortData = PortData SymbValue
type SymbPort = Port SymbValue
type SymbElemData = Port SymbValue

-- The type of values as they exist outside the SBV monad, as the strings
-- needed to retrive them from one of SBV's models 
type RefValue = Name
type RefPortData = PortData RefValue
type RefPort = Port RefValue
type RefElemData = Port RefValue

-- The type of values as they exist outside the SBV monad, as the strings
-- needed to retrive them from one of SBV's models 
type OutValue = Named Identity
type OutPortData = PortData OutValue
type OutPort = Port OutValue
type OutElemData = Port OutValue

-- | Our model of the current problem, It captures all the relevant data in 
--   the system. The UID is just for convinience, it's only really useful when
--   we're actually building stuff out and assigning the UIDs but I don't 
--   actually really care.
data Model f = Model {
    getUIDCounter  :: UID
  , getLinks       :: Map LinkUID  (ElemData f)
  , getBlocks      :: Map BlockUID (ElemData f)
  , getLinkPorts   :: Map LinkPortUID  (Port f)
  , getBlockPorts  :: Map BlockPortUID (Port f)
  , getConnections :: Map BlockPortUID (Map LinkPortUID (f Bool))
  }

makeLensesWith abbreviatedFields ''Model

-- This is where we need Undecidable instances. It's fine since we have an
-- acyclic dependency draph : Model -> ElemData -> Port -> PortData
deriving instance (Show (ElemData f), Show (Port f), Show (f Bool)) => Show (Model f)
deriving instance (Read (ElemData f), Read (Port f), Read (f Bool)) => Read (Model f)

-- | The actual CSP assembly monad, it mostly keeps track of the momentary
--   internal state of the Symbolic Monad as we build things out. 
type Symb = StateT (Model (Named SBV)) Symbolic

-- | Grab a new UID and update the counter as needed.
newUID :: Symb UID
newUID = uIDCounter <+= 1

-- | Create the relevant symbolic variable and constraints for a named input
--   value.
makeSymb :: forall a. SymWord a => NamedInputValue a -> Symb (Named SBV a)
makeSymb (Named name Unused) = undefined
makeSymb (Named name (Constraints cs)) = do
  sv <- lift $ free name
  let nv = Named name sv
  mapM_ (makeConstraint nv) cs
  return nv
  where
    -- | Add each individual constraint to the symbolic variable that already
    --   exists. 
    makeConstraint :: Named SBV b -> (String,Constraint b) -> Symb ()
    makeConstraint Named{..} (name,Is v)
      = lift . namedConstraint name $ getValue .== literal v
    makeConstraint Named{..} (name,OneOf vs)
      = lift . namedConstraint name $ sElem getValue (map literal vs)
    makeConstraint Named{..} (name,NoneOf vs)
      = lift . namedConstraint name $ bnot (sElem getValue (map literal vs))
    makeConstraint Named{..} (name,Between min max)
      = lift . namedConstraint name $ (literal min .<= getValue) &&& (getValue .<= literal max)
    makeConstraint Named{..} (name,SetFlags MaskedFlags{..})
      = lift . namedConstraint name $ (getValue .&. literal getMask) .== literal getFlags

symbPortData :: NamedInputPortData -> Symb SymbPortData
symbPortData SW{..} = SW
  <$> makeSymb getDirection
  <*> makeSymb getApi
  <*> makeSymb getApiFlags
  <*> makeSymb getApiUID
  <*> makeSymb getHostUID
symbPortData DigitalIO{..} = DigitalIO
  <$> makeSymb getDirection
  <*> makeSymb getZeroLevel
  <*> makeSymb getOneLevel
  <*> makeSymb getZeroThreshold
  <*> makeSymb getOneThreshold
  <*> makeSymb getApi
  <*> makeSymb getApiFlags
  <*> makeSymb getApiUID
symbPortData Power{..} = Power
  <$> makeSymb getDirection
  <*> makeSymb getVoltage
  <*> makeSymb getCurrentDraw
  <*> makeSymb getCurrentSupply
