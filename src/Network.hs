module Network where

import           Definitions

import           Control.Monad              (forever)
import qualified Data.ByteString.Lazy       as B

import           Data.Binary

import           Network.Transport
import           Network.Transport.InMemory
import           Network.Transport.Util     (spawn)

sendAll :: Protocol -> [Connection] -> IO ()
sendAll msg cons = mapM (\c -> send c [B.toStrict $ encode msg]) cons >> return ()

poll :: EndPoint -> (Protocol -> IO a) -> IO (Maybe a)
poll end handler = do
    res <- receive end

    case res of
        Received _ [msg] -> do
              let message = decode $ B.fromStrict msg
              Just <$> handler message
        _                -> return Nothing

pollForever :: EndPoint -> (Protocol -> IO a) -> IO ()
pollForever end handler = forever (poll end handler) >> return ()

connectOrFail :: EndPoint -> EndPointAddress -> IO Connection
connectOrFail end addr = do
  res <- connect end addr ReliableOrdered defaultConnectHints

  case res of
    Left _    -> error "Unable to connect"
    Right con -> return con

