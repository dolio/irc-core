{-# Options_GHC -Wno-unused-do-bind #-}

module Client.NetworkConnection
  ( NetworkConnection(..)
  , createConnection
  , send
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Time
import Network.Connection
import qualified Data.ByteString as B

import Irc.RateLimit
import Client.Event
import Client.Message
import Client.Connect
import Client.ServerSettings

data NetworkConnection = NetworkConnection
  { connOutQueue :: !(Chan ByteString)
  }

instance Show NetworkConnection where
  showsPrec p _ = showParen (p > 10)
                $ showString "NetworkConnection _"

send :: NetworkConnection -> ByteString -> IO ()
send c = writeChan (connOutQueue c)

createConnection ::
  NetworkName ->
  ConnectionContext ->
  ServerSettings ->
  Chan ClientEvent ->
  IO NetworkConnection
createConnection network cxt settings inQueue =
   do outQueue <- newChan

      forkIO $
        do startConnection network cxt settings inQueue outQueue
             `catch` recordFailure
           recordNormalExit

      return NetworkConnection
        { connOutQueue = outQueue
        }
  where
    recordFailure :: SomeException -> IO ()
    recordFailure ex =
      do now <- getZonedTime
         writeChan inQueue (NetworkError network now ex)

    recordNormalExit :: IO ()
    recordNormalExit =
      do now <- getZonedTime
         writeChan inQueue (NetworkClose network now)


startConnection ::
  NetworkName ->
  ConnectionContext ->
  ServerSettings ->
  Chan ClientEvent ->
  Chan ByteString ->
  IO ()
startConnection network cxt settings onInput outQueue =
  do rate <- newRateLimitDefault
     withConnection cxt settings $ \h ->
       withAsync (sendLoop h outQueue rate)      $ \sender ->
       withAsync (receiveLoop network h onInput) $ \receiver ->
         do res <- waitEitherCatch sender receiver
            case res of
              Left  Right{}  -> fail "PANIC: sendLoop returned"
              Right Right{}  -> return ()
              Left  (Left e) -> throwIO e
              Right (Left e) -> throwIO e

sendLoop :: Connection -> Chan ByteString -> RateLimit -> IO ()
sendLoop h outQueue rate =
  forever $
    do msg <- readChan outQueue
       tickRateLimit rate
       connectionPut h msg

ircMaxMessageLength :: Int
ircMaxMessageLength = 512

receiveLoop :: NetworkName -> Connection -> Chan ClientEvent -> IO ()
receiveLoop network h inQueue =
  do msg <- connectionGetLine ircMaxMessageLength h
     unless (B.null msg) $
       do now <- getZonedTime
          writeChan inQueue (NetworkLine network now (B.init msg))
          receiveLoop network h inQueue