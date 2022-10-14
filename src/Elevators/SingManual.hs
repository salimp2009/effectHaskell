{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Elevators.SingManual where
import Data.Coerce (coerce)

data DoorStateN = OpenedN | ClosedN
  deriving (Eq, Show)

data SDoorState (s::DoorStateN) where
  SClosed :: SDoorState 'ClosedN
  SOpened :: SDoorState 'OpenedN

class SDoorStateI (s::DoorStateN)  where
  sDoorState :: SDoorState s

instance SDoorStateI OpenedN where
  sDoorState = SOpened
  
instance SDoorStateI ClosedN where
    sDoorState = SClosed 
    
data Door (s::DoorStateN) where
  MkDoor :: SDoorStateI s => Door s

doorState :: forall s. Door s -> DoorStateN  
doorState MkDoor =
    case sDoorState :: SDoorState s of
        SClosed -> ClosedN
        SOpened -> OpenedN

instance Show (Door s) where
  show d = "Door " <> show (doorState d)

-- >>> doorState (MkDoor @('ClosedN)) 
-- ClosedN

-- >>>doorState (MkDoor :: Door ClosedN)
-- ClosedN

-- >>>doorState (MkDoor :: Door 'ClosedN)
-- ClosedN


openN :: Door 'ClosedN -> Door 'OpenedN
openN _ = MkDoor

closeN :: Door 'OpenedN -> Door 'ClosedN
closeN _ = MkDoor

data SomeDoor where
  SomeDoor :: Door s -> SomeDoor

deriving instance Show SomeDoor  

parseDoor :: String -> Maybe SomeDoor
parseDoor "Opened" = Just $ SomeDoor (MkDoor @('OpenedN))
parseDoor "Closed" = Just $ SomeDoor (MkDoor :: Door ClosedN)
parseDoor _        = Nothing 

switchState :: forall s. Door s -> SomeDoor
switchState door@MkDoor = 
    case sDoorState :: SDoorState s of 
        SOpened -> SomeDoor (closeN door )
        SClosed -> SomeDoor (openN door)

-- >>>switchState (MkDoor::Door 'OpenedN)
-- SomeDoor Door ClosedN

