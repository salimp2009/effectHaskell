{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--import Unsafe.Coerce (unsafeCoerce)
--import Data.Functor.Identity (Identity(..))


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
        { unsafeRunLinear :: Ix IO i j a}
        deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)

openFile :: FilePath
         -> IOMode
         -> Linear s ('LinearState next open)
                     ('LinearState (next TL.+ 1) (next ': open))
                     (Handle s next)
openFile = coerce SIO.openFile

newtype Handle s key = Handle
    { unsafeGetHandle :: SIO.Handle}

type IsOpen (key :: k) (ts::[k])  =
    IsJust =<< Find (TyEq key) ts

type Close (key :: k) (ts :: [k]) =
      Filter (Not <=< TyEq key) ts

closeFile :: Eval (IsOpen key open) ~ 'True
          => Handle s key
          -> Linear s ('LinearState next open)
                      ('LinearState next (Eval(Close key open)))
                      ()
closeFile = coerce SIO.hClose

-- >>>:t runLinear ( etcPswd >>= closeFile ) 
-- runLinear ( etcPswd >>= closeFile ) :: IO ()


-- | if the file is not closed then we a get an error;
-- >>>:t runLinear etcPswd
-- Couldn't match type: '[0]
--                with: '[]
-- Expected: Linear s ('LinearState 0 '[]) ('LinearState 1 '[]) a
--   Actual: Linear
--             s ('LinearState 0 '[]) ('LinearState (0 + 1) '[0]) (Handle s 0)


-- | if we try to close file more than once 
-- >>>:t runLinear (etcPswd >>= \f -> closeFile f >> closeFile f)
-- Couldn't match type ‘'False’ with ‘'True’
--   arising from a use of ‘closeFile’

-- >>>:t runLinear (etcPswd >>= \f -> closeFile f >> pure f) 
-- Couldn't match type ‘a’ with ‘Handle s 0’
-- Expected: Linear s ('LinearState 0 '[]) ('LinearState 1 '[]) a
--   Actual: Linear
--             s ('LinearState 0 '[]) ('LinearState 1 '[]) (Handle s 0)
--   because type variable ‘s’ would escape its scope
-- This (rigid, skolem) type variable is bound by
--   a type expected by the context:
--     forall (s :: k0).
--     Linear s ('LinearState 0 '[]) ('LinearState 1 '[]) a
--   at <interactive>:1:11-51

-- | original version was existentializing Handle to avoid leaking
-- coerce did not play well and gave error; this way is not safe
-- only using testing the rest of the implementation; it works
-- Note: Will open an issue at books repo
-- Note: there is a solution to use an explicit input parameter
-- then coerce works
runLinear :: (forall s.
              Linear s ('LinearState 0 '[] )
                       ('LinearState n '[] ) a
             )
          -> IO a
runLinear n = coerce n

-- | for testing purpose
etcPswd :: forall {k} {s :: k} {next :: Nat} {open :: [Nat]}
          . Linear
              s
              ('LinearState next open)
              ('LinearState (next TL.+ 1) (next : open))
              (Handle s next)
etcPswd = openFile "etcpasswd" ReadMode

