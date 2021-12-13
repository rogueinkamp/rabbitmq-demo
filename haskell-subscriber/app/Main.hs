#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP
import Network.AMQP (Channel)
import System.IO
import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))
import Data.Text
import           Control.Concurrent (threadDelay)

testExchange = "test-exchange"

main :: IO ()
main = do
    conn <- openConnection "rabbitmq" "/" "guest" "guest"
    ch   <- openChannel conn

    (q, _, _) <- declareQueue ch newQueue {queueName       = "python-testing",
                                           queueAutoDelete = False,
                                           queueDurable    = False}
    bindQueue ch q testExchange ""

    eventListener ch q conn

-- I docker seems like getLine does not seem to block when needed
loop :: IO ()
loop =
    do line <- getLine
       eof  <- isEOF
       putStrLn $ "output: " ++ line
       unless eof loop


eventListener :: Channel -> Text -> Connection -> IO ()
eventListener channel queue connection = do
    BL.putStrLn " [*] Waiting for messages. To exit press CTRL+C"
    consumeMsgs channel queue Ack deliveryHandler

    loop
    closeConnection connection


deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  BL.putStrLn $ " [x] HASKELL_MESSAGE_RECEIVED: " <> body
  threadDelay (1000000 * n)
  BL.putStrLn " [x] DONE"
  ackEnv metadata
  where
    body = msgBody msg
    n    = countDots body

countDots :: BL.ByteString -> Int
countDots = fromIntegral . BL.count '.'
