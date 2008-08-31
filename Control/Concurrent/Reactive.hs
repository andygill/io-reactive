{-# LANGUAGE Rank2Types #-}

-- |
-- Module: Data.Concurrent.Reactive
-- Copyright: Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: GHC
--
-- An API for generating reactive objects, as used in the TIMBER programming language.
-- 

module Control.Concurrent.Reactive 
	( Action
	, Request
	, reactiveObjectIO
	) where

import Control.Concurrent.Chan
import Control.Concurrent

-- An action is an IO-based change to an explicit state

type Action s    = s -> IO s	-- only state change
type Request s a = s -> IO (s,a) -- state change + reply to be passed back to caller

-- This is the 'forkIO' of the O'Haskell Object sub-system.
-- To consider; how do we handle proper exceptions?

reactiveObjectIO
    :: state
    -> (() -> Action state)					-- will be the exeption mechanism
    -> ( forall r . ThreadId 
	       -> (Request state r -> IO ()) 
	       -> (Action state -> IO ()) 
	       -> object
       )
    -> IO object
reactiveObjectIO state theHandler mkObject = do
  chan <- newChan

	-- the state is passed as the argument, watch for strictness issues.
  let dispatch state = do
        action <- readChan chan
        state' <- action $! state
        dispatch $! state'

	-- This trick of using a return MVar is straight from Johan's PhD.
  let requestit fun = do 
        ret <- newEmptyMVar
        writeChan chan $ \ st -> do
           (st',r) <- fun st
           putMVar ret r
	   return $ st'
        takeMVar ret

  let actionit = writeChan chan

   	-- We return the pid, so you can build an abort function
  pid <- forkIO $ dispatch state

  return $ mkObject pid requestit actionit






