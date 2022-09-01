{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module LinearAllocations where

import Control.Monad.Indexed
import Data.Coerce
import Fcf
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import IndexedMonadsExample
import Language.Haskell.DoNotation
import Prelude hiding (Monad (..), pure)
import qualified System.IO as SIO
import System.IO hiding (openFile, Handle)




