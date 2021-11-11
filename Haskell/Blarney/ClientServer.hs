{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

{-|
Module      : Blarney.ClientServer
Description : Client and Server interfaces for control flow
Copyright   : (c) Matthew Naylor 2021, Alexandre Joannou 2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module defines the 'Client' and 'Server' types.

-}
module Blarney.ClientServer where

-- Standard imports
import GHC.Generics
import Control.Monad hiding (when)

-- Blarney imports
import Blarney
import Blarney.SourceSink
import Blarney.Connectable

-- Interfaces
-- ==========

-- | Clients produce requests and consume responses
data Client req_t resp_t =
  Client {
    reqs :: Source req_t
  , resps :: Sink resp_t
  } deriving (Generic, Interface)

-- | Servers consume requests and produce responses
data Server req_t resp_t =
  Server {
    reqs :: Sink req_t
  , resps :: Source resp_t
  } deriving (Generic, Interface)

-- Instances
-- =========

instance (Bits req_t, Bits resp_t) =>
           Connectable (Client req_t resp_t) (Server req_t resp_t) where
  makeConnection c s = do
    makeConnection c.reqs s.reqs
    makeConnection s.resps c.resps

-- Helpers
-- =======

-- | Add debug displays to client's source and sink
debugClient :: (FShow req_t, FShow resp_t) =>
                 Client req_t resp_t -> Format -> Client req_t resp_t
debugClient c msg =
  Client {
    reqs = debugSource c.reqs msg
  , resps = debugSink c.resps msg
  }

-- | Add debug displays to servers's source and sink
debugServer :: (FShow req_t, FShow resp_t) =>
                 Server req_t resp_t -> Format -> Server req_t resp_t
debugServer s msg =
  Server {
    reqs = debugSink s.reqs msg
  , resps = debugSource s.resps msg
  }
