
-- | This is where functions related to creating and playing with the input 
--   formats for Ports and Elements
module Input where

import API
import Types
import Data.SBV
import Data.Monoid ((<>))
import Data.Bifunctor

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
type NamedInputValue   = Constraints ((,) String)
type NamedInputPortData  = PortData NamedInputValue
type NamedInputPort      = Port     NamedInputValue
type NamedInputElemData  = ElemData NamedInputValue

nameConstraint :: (Show a)
               => String
               -> (Maybe String, Constraint a)
               -> (String, Constraint a)
nameConstraint context (mName,constraint)
  = (context ++ ": " ++ name mName constraint, constraint)
  where
    name (Just s) _ = s
    name _        c = show c

nameConstraints :: (Show a)
                => String
                -> InputValue a
                -> NamedInputValue a
nameConstraints _ Unused = Unused
nameConstraints context (Constraints cl) 
  = Constraints $ zipWith (nameConstraint . appendContext) [1..] cl
  where
    appendContext i = context ++ ".contraint[" ++ show i ++ "]"

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

-- | Tiny string assembly utility function, just assembles two strings with a 
--   dot in between
(+.+) :: String -> String -> String
a +.+ b = a ++ "." ++ b

