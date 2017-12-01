{-# LANGUAGE OverloadedStrings #-}

module Comms.SMTP.State where

import           Comms.Types
import           Control.Concurrent.STM
import           Data.Monoid            ((<>))
import qualified Data.Text              as T

updateEnvState :: EnvelopeMVar -> SMTPState -> IO ()
updateEnvState mvar newState = do
  putStrLn "Entered updateEnvState"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateEnvState - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateEnvState - Nothing Case."
      overwriteEnvelope mvar (Envelope "" "" [] newState "")
    Just oldE -> do
      putStrLn "updateEnvState - Just Case."
      overwriteEnvelope mvar (oldE {state = newState})

updateEnvFrom :: EnvelopeMVar -> T.Text -> IO ()
updateEnvFrom mvar from = do
  putStrLn "Entered updateEnvFrom"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateEnvFrom - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateEnvFrom - Nothing Case."
      overwriteEnvelope mvar (Envelope "" from [] HaveMailFrom "")
    Just oldE -> do
      putStrLn "updateEnvFrom - Just Case."
      overwriteEnvelope mvar (oldE {from = from})

updateEnvTo :: EnvelopeMVar -> T.Text -> IO ()
updateEnvTo mvar newTo = do
  putStrLn "Entered updateEnvTo"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateEnvTo - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateEnvTo - Nothing Case."
      overwriteEnvelope mvar (Envelope "" "" [newTo] HaveRcptTo "")
    Just oldE -> do
      putStrLn "updateEnvTo - Just Case."
      overwriteEnvelope mvar (oldE {to = newTo : to oldE})

updateEnvContents :: EnvelopeMVar -> T.Text -> IO ()
updateEnvContents mvar newContents = do
  putStrLn "Entered updateEnvContents"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateEnvContents - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateEnvContents - Nothing Case."
      overwriteEnvelope mvar (Envelope "" "" [] HaveRcptTo newContents)
    Just oldE -> do
      putStrLn "updateEnvContents - Just Case."
      overwriteEnvelope mvar (oldE {contents = contents oldE <> newContents})

newEmptyEnvelopeMVar :: IO EnvelopeMVar
newEmptyEnvelopeMVar = atomically newEmptyTMVar

{- | Swaps the value -}
updateEnvelope :: EnvelopeMVar -> Envelope -> IO ()
updateEnvelope env = atomically . putTMVar env

{- | Replaces contents of the TMVar with the supplied value.
Non-Blocking.
-}
overwriteEnvelope :: EnvelopeMVar -> Envelope -> IO ()
overwriteEnvelope mvar env = do
  old <- atomically $ tryTakeTMVar mvar
  case old of
    Nothing -> atomically $ putTMVar mvar env
    Just _  -> atomically $ putTMVar mvar env

getEnvelope :: EnvelopeMVar -> IO Envelope
getEnvelope = atomically . takeTMVar
