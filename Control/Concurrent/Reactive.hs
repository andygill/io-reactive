{-# LANGUAGE Rank2Types #-}
module Control.Concurrent.Reactive where

import Control.Concurrent.Chan
import Control.Concurrent


-- This library allows the user to build reactive objects, based on -- the ideas from Timber and O'Haskell.

-- An action is an IO-based change to an explicit state
type Action s    = s -> IO s	-- only state change
type Request s a = s -> IO (s,a) -- state change + reply to be passed back to

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

  -- The dispatch loop

  let dispatch state = do
        action <- readChan chan
        state' <- action state
        dispatch state'

  let requestit fun = do 
        ret <- newEmptyMVar
        writeChan chan $ \ st -> do
           (st',r) <- fun st
           putMVar ret r
	   return $ st'
        takeMVar ret


  let actionit = writeChan chan

  pid <- forkIO $ dispatch state

  return $ mkObject pid requestit actionit






