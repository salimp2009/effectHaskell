{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
module Elevators.SingGen where

import Data.Singletons.TH
import Prelude.Singletons

singletons [d|
  data DoorStateM 
    = OpenedM 
    | ClosedM
    deriving Show
  |]

data DoorM (s :: DoorStateM) where
  MkDoorM :: SingI s => DoorM s 


doorStateM :: forall s. DoorM s -> DoorStateM
doorStateM MkDoorM =
  case sing :: SDoorStateM s of
    SClosedM -> ClosedM
    SOpenedM -> OpenedM

-- >>> doorStateM (MkDoorM @('ClosedM))     
-- ClosedM

-- >>> doorStateM (MkDoorM :: DoorM 'ClosedM)  
-- ClosedM

-- >>> doorStateM (MkDoorM :: DoorM 'OpenedM) 
-- OpenedM


    
    

  


