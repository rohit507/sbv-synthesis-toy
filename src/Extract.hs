
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

-- ## Extract.hs ##
-- extractPortData
-- extractPort
-- extractElem
-- extractEdge
-- extractModel
