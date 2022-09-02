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


-- | goal is to use IxMonad which allows 
-- you open file handles and closes each of them exactly one time
-- therfore we need to track whether a file is closed or open at type level
-- need a type level list of open files, when a file is open it will be added
-- when closed it will be deleted from list and also need unique keys for file handles
-- a strictly increasing Nat can do it

-- | LinearState exist to be used as a data kind ; 
-- it will be serve as the "index" of Indexed Monad
{- 
  "Any given monadic operation will be parameterized by
  the LinearState going into it and the LinearState coming
  out of it."
-}
data LinearState = LinearState
          { linearNextKey  :: Nat
          , linearOpenKeys :: [Nat]
          }
-- | s type parameter is used as ST trcik to prevent file handles leaking out
{- 
  "unsafeRunLinear is unsafe in two ways
  — it lets us run arbitrary Linear computations, including incomplete
  ones in which we haven’t yet closed all of our file
  handles. 
  - Additionally, it doesn’t existentialize s,
  meaning file handles can leak out of it"
-}
newtype Linear s (i::LinearState) (j::LinearState) a = Linear
        { unsafeLinear :: Ix IO i j a}
        deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)

openFile :: FilePath 
         -> IOMode 
         -> Linear s ('LinearState next open) 
                     ('LinearState (next TL.+ 1) (next ': open))
                     (Handle s next)    
openFile = coerce SIO.openFile
                     
newtype Handle s key = Handle
    { unsafeGetHandle :: SIO.Handle}                     
