{-# LANGUAGE RankNTypes, GADTs #-}

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
import Control.Exception as Ex

-- An action is an IO-based change to an explicit state

type Action s    = s -> IO s	-- only state change
type Request s a = s -> IO (s,a) -- state change + reply to be passed back to caller

-- This is the 'forkIO' of the O'Haskell Object sub-system.
-- To consider; how do we handle proper exceptions?
--   we need to bullet-proof this for exception!

-- Choices:
--   * do the Requests see the failure
--   * Actions do not see anything
--   * 

data Msg s = Act (Action s)
           | forall a . Req (Request s a) (MVar a)
           | Done (MVar ())

reactiveObjectIO
    :: state
    -> (
	   ThreadId 
	-> (forall r. Request state r -> IO r) 	-- requests
	-> (Action state -> IO ()) 		-- actions
        -> IO ()				-- done
	-> object
       )
    -> IO object
reactiveObjectIO state mkObject = do
  chan <- newChan

	-- the state is passed as the argument, watch for strictness issues.
  let dispatch state = do
        action <- readChan chan
        case action of
          Act act -> do state1 <- act state
                        dispatch $! state1
          Req req box -> do (state1,ret) <- req state
                            putMVar box ret
                            dispatch $! state1
          Done box -> do putMVar box ()
                         return ()	-- no looping; we are done

   	-- We return the pid, so you can build a hard-abort function
        -- we need to think about this; how do you abort an object
  pid <- forkIO $ dispatch state

	-- This trick of using a return MVar is straight from Johan's PhD.
  let requestit fun = do 
        ret <- newEmptyMVar
        writeChan chan $ Req fun ret
        takeMVar ret	-- wait for the object to react

  let actionit act = writeChan chan $ Act act

  let doneit = do
        ret <- newEmptyMVar
        writeChan chan $ Done ret
        takeMVar ret	-- wait for the object to *finish*

  return (mkObject pid requestit actionit doneit)

