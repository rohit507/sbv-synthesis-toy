
-- | This is where functions related to creating and playing with the input 
--   formats for Ports and Elements
module Input where

import API
import Types
import Data.SBV
import Data.Monoid ((<>))
import Data.Bifunctor

import Data.Map (Map)
import qualified Data.Map as Map

-- The most general type for a lot of the input constraints, allows you to 
-- specificy a name where applicable.
type InputValue   = Constraints ((,) (Maybe String))
type InputPortData  = PortData InputValue
type InputPort      = Port InputValue
type InputElemData  = ElemData InputValue

{-
testPort :: InputPort
testPort = Port {
  getName = "Test",
  getUID = Unused,
  getUsed = Unused,
  getConnected = Unused,
  getConnectedUID = Unused,
  getData = Power {
    getDirection = unnamed [Is Sink],
    getVoltage = Constraints [(Nothing,Between 3.2 3.4)],
    getCurrentDraw = Constraints [(Nothing,Between 1.2 2.4)],
    getCurrentSupply = Unused
  }
}
-}

-- | Given a list of unnamed constraints, convert them into an 
--   InputValue
unnamed :: SymWord a => [Constraint a] -> InputValue a
unnamed = Constraints . map (Nothing,)

-- | Given a list of named constraints, convert them into an 
--   Inputvalue. 
named :: SymWord a => [(String,Constraint a)] -> InputValue a
named = Constraints . map (first Just)


instance SymWord a => Monoid (InputValue a) where
  mempty = Constraints []
  mappend (Constraints al) (Constraints bl) = Constraints $ al ++ bl
  -- There's no really good semantics for how Unused and Constraints interact.
  -- It's basically just undefined behavior, and our decision here is arbitary.
  mappend _ _ = Unused

-- UIDs are only going to be assigned when we insert the element into the
-- design, so we'll just stick a function in there.
type NamedInputValue   = Named (Constraints ((,) String))
type NamedInputPortData  = PortData NamedInputValue
type NamedInputPort      = Port     NamedInputValue
type NamedInputElemData  = ElemData NamedInputValue

-- | Give each constraint a more-useful, unique name
nameConstraint :: (Show a)
               => String
               -> (Maybe String, Constraint a)
               -> (String, Constraint a)
nameConstraint context (mName,constraint)
  = (context ++ " : " ++ name mName constraint, constraint)
  where
    name (Just s) _ = s
    name _        c = show c

-- | Give the set of constraints on a single value more useful names.
nameConstraints :: (Show a)
                => String
                -> InputValue a
                -> NamedInputValue a
nameConstraints context Unused = Named context Unused
nameConstraints context (Constraints cl) 
  = Named context . Constraints $ zipWith (nameConstraint . appendContext) [1..] cl
  where
    appendContext i = context -- ++ ".contraint[" ++ show i ++ "]"

-- TODO :: Do this with a functor morphism/traversal or something? The big
--         issue is figuring out if there's any good laws that will let me 
--         handle the recursion gracefully.

-- | Give each field in the portData a more useful unique name.
namePortData :: String -> InputPortData -> NamedInputPortData
namePortData context SW{..} = SW {
    getDirection = (nameConstraints . (context +.+)) "direction" getDirection
  , getApi       = (nameConstraints . (context +.+)) "api"       getApi
  , getApiFlags  = (nameConstraints . (context +.+)) "apiFlags"  getApiFlags
  , getApiUID    = (nameConstraints . (context +.+)) "apiUID"    getApiUID
  , getHostUID   = (nameConstraints . (context +.+)) "hostUID"   getHostUID
  }
namePortData context DigitalIO{..} = DigitalIO {
    getDirection = (nameConstraints . (context +.+)) "direction" getDirection
  , getZeroLevel = (nameConstraints . (context +.+)) "zeroLevel" getZeroLevel
  , getOneLevel = (nameConstraints . (context +.+)) "oneLevel" getOneLevel
  , getZeroThreshold = (nameConstraints . (context +.+)) "zeroThreshold" getZeroThreshold
  , getOneThreshold = (nameConstraints . (context +.+)) "zeroThreshold" getOneThreshold
  , getApi       = (nameConstraints . (context +.+)) "api"       getApi
  , getApiFlags  = (nameConstraints . (context +.+)) "apiFlags"  getApiFlags
  , getApiUID    = (nameConstraints . (context +.+)) "apiUID"    getApiUID
  }
namePortData context Power{..} = Power {
    getDirection = (nameConstraints . (context +.+)) "direction" getDirection
  , getVoltage = (nameConstraints . (context +.+)) "voltage" getVoltage
  , getCurrentDraw = (nameConstraints . (context +.+)) "currentDraw" getCurrentDraw
  , getCurrentSupply = (nameConstraints . (context +.+)) "currentSupply" getCurrentSupply
  }

-- | Adding a more useful name to each port, only do this after a uid has
--   been assigned. 
namePort :: String -> InputPort -> NamedInputPort
namePort context p@Port{..} = p{
    getUID = (nameConstraints . (context' +.+)) "uid" getUID
  , getUsed = (nameConstraints . (context' +.+)) "used" getUsed
  , getConnected = (nameConstraints . (context' +.+)) "connected" getConnected
  , getConnectedUID = (nameConstraints . (context' +.+)) "connectedUID" getConnectedUID
  , getPortData = namePortData context' getPortData
  }
  where
    context' = case getRawUID of
      Just i -> context ++ brk i
      -- You should only be calling this after the UID has been assigned 
      Nothing -> undefined

-- | Adding more useful names for each element, only do this after UIDs have 
--   been assigned.
nameElemData :: String -> InputElemData -> (String,NamedInputElemData)
nameElemData context e@ElemData{..} = (,) context' $ e {
    getUID = (nameConstraints . (context' +.+)) "uid" getUID
  , getUsed = (nameConstraints . (context' +.+)) "used" getUsed
  , getPorts = Map.mapWithKey (namePort . (context' +.+)) getPorts
  }
  where
    context' = case getRawUID of
      Just i -> context ++ brk i
      -- You should only be calling this after the UID has been assigned 
      Nothing -> undefined

-- | Given an action that extracts a UID add one to the design, also fills 
--   in various fields. 
initPort :: Monad m => m UID -> InputPort -> m InputPort
initPort genUID p@Port{..} = do
  uid <- case getRawUID of
    Just i -> return i
    Nothing -> genUID
  -- *sigh* The `getPortData` bit here just lets us forgo the manual annotation
  -- needed to get GHC to understand that we're talking about `Port`s here. 
  return $ p{
      getRawUID = Just uid,
      getUID = unnamed [Is uid],
      getUsed = unnamed [],
      getConnected = unnamed [],
      getConnectedUID = unnamed [],
      getPortData = getPortData
    }

-- | Given an action that extracts a UID, add them to the elements and ports. 
--   Also fills in various fields.
initElemData :: Monad m => m UID -> InputElemData -> m InputElemData
initElemData genUID e@ElemData{..} = do
  -- I'd like to keep all the UIDs for elems and their ports as 
  -- contiguous as possible. 
  uid <- case getRawUID of
    Just i -> return i
    Nothing -> genUID
  -- Keeping this separate ensures we're not assuming that the existence of a
  -- UID for the Elem implies the existence of a UID for each port. 
  ports <- traverse (initPort genUID) getPorts
  return $ e{
      getRawUID = Just uid,
      getUID = unnamed [Is uid],
      getUsed = unnamed [],
      getPorts = ports
    }


-- | Tiny string assembly utility function, just assembles two strings with a 
--   dot in between
(+.+) :: String -> String -> String
a +.+ b = a ++ "." ++ b

-- | Another helper, this time for wrapping a uid up in brackets.
brk :: Show a => a -> String
brk = (++ "]") . ("[" ++) . show
