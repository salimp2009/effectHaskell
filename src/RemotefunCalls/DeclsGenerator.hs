{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module RemotefunCalls.DeclsGenerator where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Network.Socket (PortNumber)

import RemotefunCalls.RemoteParser

remote = QuasiQuoter 
        {
          quoteExp = undefined
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = quoteFuncInfoDec
        }

quoteFuncInfoDec :: String -> Q [Dec]
quoteFuncInfoDec quote = parseRemoteInterface quote >>= genClientStubs

