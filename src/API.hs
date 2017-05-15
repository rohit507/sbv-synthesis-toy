
-- | Lots of types and things that relevant to APIs/Element types used by 
--   our system. 
module API where

import Data.SBV
import Data.Data
import Data.Bits

-- | The direction for various interfaces. 
--
--   - SW        : Source = API Provider  , Sink = API User 
--   - DigitalIO : Source = Signal Output , Sink = Signal Input
--   - Power     : Source = Power Supply  , Sink = Power Load
data Direction = Source | Sink | Bidir
  deriving (Eq, Ord, Enum, Show, Read, Data, SymWord, HasKind, SatModel)

-- | Types for different kinds of SW interaces that may be relevant. 
--
--   NOTE :: Our actual synthesis tool carries much more information
--           than a simple enum allows. 
data Api = LED | Button | Thermometer | Motor | LightSensor | DistanceSensor
  | Fan | Display
  deriving (Eq, Ord, Enum, Show, Read, Data, SymWord, HasKind, SatModel)

-- | We end up storing various flags over the API as a bitvector. 
type FlagSet = Int32

-- Specific flags we use for LEDs
data LEDFlags
  = LRed | LGreen | LBlue | LYellow | LPurple | LCyan | LWhite | LRGB -- Colors
  | LDim | LBright -- Brightness 
  | LSOT | LDIP | LOther -- Size of the actual LED
  deriving (Enum, Bounded, Show, Read)

-- Specific Flags we user for buttons 
data ButtonFlags
  = BRed | BGreen | BBlue | BYellow | BPurple | BCyan | BWhite -- Colors
  | BLit -- Internal Lighting
  | BPushbutton | BKey | BDome | BTrigger -- The Style of the button
  deriving (Enum, Bounded, Show, Read)

-- Specific Flags we user for buttons 
data ThermometerFlags
  = TWaterproof | TRanged -- Properties the thermometer may have
  deriving (Enum, Bounded, Show, Read)

-- Specific Flags we use for Motors
data MotorFlags
  = MStepper | MServo | MBrushed | MBrushless -- Type of motor this is
  deriving (Enum, Bounded, Show, Read)

-- Specific flags for LightSensors 
data LightSensorFlags
  = LSBrightness | LSColor -- Type of sensing it can do
  | LSInfrared | LSVisible | LSNoSource -- Type of built in light source
  deriving (Enum, Bounded, Show, Read)

-- Specific flags for LightSensors 
data DistanceSensorFlags
  = DShort | DSMed | DSLong -- Range
  | DSUltrasonic | DSTimeOfFlight | DSReflective | DSLIDAR | DSStereo -- Mode of operation
  deriving (Enum, Bounded, Show, Read)

-- Specific flags for Fans
data FanFlags
  = FSmall | FMed | FLarge -- Size
  | FHighPressure | FLowPressure -- Pressure 
  | FHighThroughput | FLowThroughput -- Volume of air moved per unit time
  deriving (Enum, Bounded, Show, Read)

-- Specific flags for displays 
data DisplayFlags
  = DCharecter | DNumerical | DGraphical -- Type
  | DBlackAndWhite | DColor | DLightOnDark | DDarkOnLight -- Color
  deriving (Enum, Bounded, Show, Read)

-- Get all the valid flags from the bitvector we use to store them.
fromFlags :: (Enum a, Bounded a) => FlagSet -> [a]
fromFlags fls = filter (testBit fls . fromEnum) (enumFromTo minBound maxBound)

data MaskedFlags = MaskedFlags {getMask :: FlagSet, getFlags :: FlagSet}
  deriving (Eq, Show, Read)

-- Turn a list of flags and their values into a bitvector and mask
toFlags :: (Enum a, Bounded a) => [(a,Bool)] -> MaskedFlags
toFlags = foldl add (MaskedFlags 0 0)
  where
  add MaskedFlags{..} (fromEnum -> a, True )
    = MaskedFlags{ getMask = setBit getMask a,getFlags = setBit   getFlags a}
  add MaskedFlags{..} (fromEnum -> a, False)
    = MaskedFlags{ getMask = setBit getMask a,getFlags = clearBit getFlags a}
