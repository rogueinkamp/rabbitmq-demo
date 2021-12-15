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
    putStrLn "CONNECTING..."
    conn <- openConnection "rabbitmq" "/" "guest" "guest"
    ch   <- openChannel conn

    (q, _, _) <- declareQueue ch newQueue {queueName       = "python-testing",
                                           queueAutoDelete = False,
                                           queueDurable    = False}

    declareExchange ch newExchange {exchangeName = testExchange, exchangeType = "direct"}
    bindQueue ch q testExchange ""

    putStrLn "RUNNING..."
    eventListener ch q conn


eventListener :: Channel -> Text -> Connection -> IO ()
eventListener channel queue connection = do
    BL.putStrLn " [*] Waiting for messages. To exit press CTRL+C"
    consumeMsgs channel queue Ack deliveryHandler

    forever (getLine >>= putStrLn)
    closeConnection connection


deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  BL.putStrLn $ "HASKELL_MESSAGE_RECEIVED -> " <> body
  threadDelay (1000000 * n)
  ackEnv metadata
  where
    body = msgBody msg
    n    = countDots body

countDots :: BL.ByteString -> Int
countDots = fromIntegral . BL.count '.'
