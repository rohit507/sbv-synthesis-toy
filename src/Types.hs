{-# LANGUAGE UndecidableInstances #-}

module Types where

import API

import Data.Map (Map)
import qualified Data.Map as Map

import Data.SBV

import Control.Lens

-- * Type Synonyms 

-- | Type that we use to represent unique IDs used in all sorts of places.
type UID = Integer
-- These are just to help disambiguate the types a bit since we use UIDs for 
-- a variety of different things. 
type PortUID      = UID
type LinkUID      = UID
type BlockUID     = UID
type LinkPortUID  = UID
type BlockPortUID = UID

-- * Names 

-- | Used to keep track of the various names we assign to things
newtype Name a = Name {getName :: String}
  deriving (Show, Read)

-- | Type with both a name and an associated value of some sort.
data Named f a = Named {getName :: String, getValue :: f a}
  deriving (Show, Read)

-- | Swap the inner data without changing the name. 
swapNamed :: (f a -> g b) -> Named f a -> Named g b
swapNamed f (Named n v) = Named n (f v)

-- | Takes a named item, strips the actual value, and returns just the name.
toName :: Named f a -> Name a
toName Named{ getName} = Name getName

-- * Constraints

-- | The datatype that captures certain types of contraints and ambiguity.
--
-- NOTE :: Our actual tool has a wider of range of contraints can can work 
--         with them algebraically before converting them into elements of 
--         an SMT problem.
--
data Constraint a where
  -- We know what the value is 
  Is      :: (SymWord a) => a -> Constraint a
  -- The value is one of these
  OneOf   :: (Eq a , SymWord a, EqSymbolic  (SBV a)) => [a] -> Constraint a
  -- The value is not any of these
  NoneOf  :: (Eq a , SymWord a, EqSymbolic  (SBV a)) => [a] -> Constraint a
  -- The value is within in this range
  -- (minmum then maximum. To avoid the haskell-src-exts parse error. :/)
  Between :: (Ord a, SymWord a, OrdSymbolic (SBV a)) => a -> a -> Constraint a
  -- The flags that happen to be specified for this tool. 
  SetFlags :: (a ~ FlagSet) => MaskedFlags -> Constraint a

deriving instance (Show a) => Show (Constraint a)

-- | Multiple constraints that must all be fulfilled. 
data Constraints f a where
  -- We don't use this value in this stage of the process. 
  Unused :: Constraints f a
  -- The set of constraints over this value that must all apply
  Constraints :: (SymWord a) => [f (Constraint a)] -> Constraints f a

deriving instance (Show (f (Constraint a))) => Show (Constraints f a)


-- | This whole thing is meant to allow us to easily capture all the
--   states used for each item in our actual design. 
--
--   PortData (Constraints (String,)) / Port (Constraints (String,)) / Elem (Constraints (String,)) = 
--      Basic sets of constraints over the values in each type of object, 
--      along with the names we'll use for those constraints. 
--
--   PortData (Named SBV) / Port (Named SBV) / Elem (Named SBV) =
--      The names we use to extract information and the internal SBV Variables 
--
--   PortData Name / Port Name / Elem Name =
--      Just the names, for when we need to extract our output
--
--   PortData Identity / Port Identity / Elem Identity = 
--      Just the final assigned values for everything, our output

-- | Data specific to the various kinds of ports that we end up using.
--
--   NOTE :: In an actual system these types would all be dealt with at runtime
--           to allow the library author to choose them as needed.
data PortData f
  = SW {
      getDirection :: f Direction,
      -- The Type of interface being provided.
      getApi       :: f Api,
      -- Flags that tell us about the API/Data being transferred, generally 
      -- specific to each API type. 
      getApiFlags  :: f FlagSet,
      -- The UID of the thing the API is connecting to.
      getApiUID    :: f BlockUID,
      -- The UID of the host processor that the library is running on
      getHostUID   :: f BlockUID,
      -- Is this just the digital IO interface for this data, or is it the 
      -- final dive-class specific interface? 
      getIsGPIO    :: f Bool
    }
  | DigitalIO {
      getDirection :: f Direction,
      -- The voltage of the signal when the output is zero.
      getZeroLevel :: f Float,
      -- The voltage of the signal when the output is one.
      getOneLevel :: f Float,
      -- The maximum voltage which the input will treat as a zero.
      getZeroThreshold :: f Float,
      -- The minimum voltage which the input will treat as a one. 
      getOneThreshold :: f Float,
      -- The type of data being transferred
      getApi :: f Api,
      -- The flags of the thing connected
      getApiFlags :: f FlagSet,
      -- The UID of the thing connected
      getApiUID :: f BlockUID
    }
  | Power {
      getDirection :: f Direction,
      -- Voltage for input or output.
      getVoltage :: f Float,
      -- The current draw of any load
      getCurrentDraw :: f Float,
      -- The maximum current draw for any power supply. 
      getCurrentSupply :: f Float
    }


-- This needs undecidable instances, and I'm too lazy to write the version
-- that would work without it. 
deriving instance (Show (f Direction), Show (f Api), Show (f FlagSet), Show (f Float), Show (f UID), Show (f Bool)) => Show (PortData f)
deriving instance (Read (f Direction), Read (f Api), Read (f FlagSet), Read (f Float), Read (f UID), Read (f Bool)) => Read (PortData f)

-- Data that is neccesary for every port 
data Port f = Port {
    -- The name we're using for the port. The Input data, this is generic, but
    -- when we name the port it'll be the specific instance name. 
    getName :: String,
    -- What is the port's UID? 
    getRawUID :: Maybe PortUID,
    getUID :: f UID,
    -- Is the port being used in the design? 
    getUsed :: f Bool,
    -- Is the port connected to something? 
    getConnected :: f Bool,
    -- What is the port connected to? 
    getConnectedUID :: f PortUID,
    -- What other information do we have about the port? 
    getPortData :: PortData f
  }


-- This needs undecidable instances, and I'm too lazy to write the version
-- that would work without it. 
deriving instance (Show (PortData f), Show (f Bool), Show (f UID)) => Show (Port f)
deriving instance (Read (PortData f), Read (f Bool), Read (f UID)) => Read (Port f)

-- | This can be either a block or a link, depending on where in the design
--   it's being used. 
data Elem f = Elem {
    -- The name we're using for the elem. The Input data, this is generic, but
    -- when we name the elem it'll be the specific instance name. 
    getName :: String,
    -- The UID of this block or link
    getRawUID :: Maybe UID,
    getUID :: f UID,
    -- Whether the block or link is part of the output design
    getUsed :: f Bool,
    -- The element's ports
    getPorts :: Map String (Port f)
  }


-- This needs undecidable instances, and I'm too lazy to write the version
-- that would work without it. 
deriving instance (Show (Port f), Show (f Bool), Show (f UID)) => Show (Elem f)
deriving instance (Read (Port f), Read (f Bool), Read (f UID)) => Read (Elem f)


makeLensesWith abbreviatedFields ''Name
makeLensesWith abbreviatedFields ''Named
makeLensesWith abbreviatedFields ''PortData
makeLensesWith abbreviatedFields ''Port
makeLensesWith abbreviatedFields ''Elem
