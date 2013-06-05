
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pipes.CEP where


import Control.Monad hiding (mapM_)

import Control.Proxy
import Control.Proxy.Concurrent
import qualified Control.Proxy.Trans.State as S

import Control.Concurrent.Async

import Data.Time.Clock.POSIX
import Data.List 
import Data.Monoid

import Data.Foldable (mapM_)

import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

import Prelude hiding (mapM_)


--------------------------------------------------


--------------------------------------------------

type TimeWindowState a = [(POSIXTime, a)]

-- | create a window about n seconds
-- note that the first element in the resulting list
-- is the newest
windowTime :: Proxy p => POSIXTime -> () -> Pipe p a [a] IO ()
windowTime = S.evalStateK [] . go 
  where 
    go w () = forever $ do

   
      -- get current time, current value,
      -- and current window
      v <- request ()
      s <- S.get
      t <- lift getPOSIXTime

      -- append new value to the front of our buffer
      -- and take elements from buffer that aren't
      -- older than the given (w)indow
      let s' = takeWhile (\(t',_) -> t' > t - w) $ (t,v):s

      -- update state
      S.put s'

      -- get current buffer and respond it
      respond $ map snd s'


--------------------------------------------------

-- | create a window above the last n events
-- note that the first element in the resulting list
-- is the newest
windowLength :: (Proxy p, Monad m) => Int -> () -> Pipe p a [a] m ()
windowLength = S.evalStateK [] . go 
  where
    go l () = forever $ do

      -- get current value and window
      v <- request ()
      s <- S.get

      -- append new value to front of our buffer
      -- and ensure buffer max length
      let s' = take l $ v:s

      -- update state
      S.put s'

      -- responst current buffer
      respond s'


--------------------------------------------------

-- | count every event that is going through this proxy
countD :: (Proxy p, Monad m) => () -> Pipe p a Int m ()
countD () = runIdentityP $ loop 0
  where 
    loop c = do
      _ <- request ()
      let c' = c + 1
      _ <- respond c'
      loop c'

--------------------------------------------------

-- | count events going through this proxy and calculate
-- | the rate over a set time window
rateD :: (Proxy p) => POSIXTime -> () -> Pipe p a Double IO ()
rateD t = windowTime t >-> mapD (\as -> genericLength as * factor)
  where factor = 1e12 / fromIntegral (fromEnum t)
    

--------------------------------------------------




-- | output the last event of a given time interval 
outputLastEvery :: (Proxy p) => POSIXTime -> () -> Pipe p a a IO ()
outputLastEvery period () = runIdentityP $ do  
    t <- lift getPOSIXTime 
    loop (t + period) Nothing
  where 
    loop nextT _ = do
      v <- request ()
      t <- lift getPOSIXTime

      when (nextT < t) $ do
        _ <- respond v
        loop (t + period) Nothing 

      loop nextT (Just v)

-- | output all events from a given time interval
outputAllEvery :: (Proxy p) => POSIXTime -> () -> Pipe p a a IO ()
outputAllEvery period () = runIdentityP $ do
    t <- lift getPOSIXTime
    loop (t + period) Seq.empty
  where
    loop nextT buf = do
      v <- request ()
      t <- lift getPOSIXTime

      let buf' = buf |> v

      when (nextT < t) $ do
        mapM_ respond buf'
        loop (t + period) Seq.empty
        
        

      loop nextT buf'


