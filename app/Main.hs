{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)

import qualified Data.Massiv.Array          as A

import           Data.IORef

import           Network.Transport
import           Network.Transport.InMemory
import           Network.Transport.Util     (spawn)

import           Control.Monad.State        as S

import           Definitions
import           Lib
import           Network
import           Random
import           Utils

-- TODO : Command Line Interface

main :: IO ()
main = setup 4 -- Use 4 cluster nodes

supervisor :: Vertex -> EndPointAddress -> [EndPointAddress] -> EndPoint -> IO ()
supervisor home displayerAddr addresses end = do

    cons <- mapM (connectOrFail end) addresses
    conDisplay <- connectOrFail end displayerAddr

    let resolver vertex = cons !! (vertex `div` n)

    let handler (TransferRequest ants) =
            forM_ ants $ \ant -> sendAll (AntTransfer ant) [resolver (vertex ant)]


    forkIO $ pollForever end handler

    forM_ [0..] $ \i -> do

        if i `mod` 60 == 0 then
            -- Add 5 ants
            forM_ [1..5] . const $ sendAll (AntTransfer $ newAnt home) [resolver home]
        else
          return ()

        threadDelay (deltaT `div` 2)
        putStrLn $ "Step " ++ show i
        sendAll Step cons
        sendAll (DisplayRequest displayerAddr) cons
        threadDelay (deltaT `div` 2)
        sendAll (FlushDisplay i) [conDisplay]

displayer :: EndPoint -> IO ()
displayer end = pollForever end handler

  where handler (DisplayResponse resp) = do
            appendFile "graph.tmp" resp

        handler (FlushDisplay i) = do
            let path = "graphs/graph" ++ show i ++ ".gv"
            writeFile path $ "digraph graph" ++ show i ++ " {\n"
            readFile "graph.tmp" >>= appendFile path
            appendFile path "\n}"
            writeFile "graph.tmp" ""

        handler _ = return ()

worker :: EndPointAddress -> WorkerConfig -> EndPoint -> IO ()
worker addr config end = do

    conSupervisor <- connectOrFail end addr
    ref <- newIORef Nothing

    let handler Step = do
              toTransfer <- runWorker config ref move
              sendAll (TransferRequest toTransfer) [conSupervisor]

        handler (AntTransfer ant) = runWorker config ref (accomodate ant)

        handler (DisplayRequest addr) = do
              response <- runWorker config ref getDisplay
              displayCon <- connectOrFail end addr
              sendAll (DisplayResponse response) [displayCon]

        handler _ = return ()

    pollForever end handler


setup :: Int -> IO ()
setup clusterNodes = do

    writeFile "graph.tmp" ""

    transport <- createTransport -- TODO: use network-transport-amqp instead
    Right end <- newEndPoint transport

    let supervisorAddr = address end

    let ((entireGraph,home,food),seed) = (flip runState) 42 $ do
          graphs <- forM [1..clusterNodes] $ const completeGraph

          let graph = foldl1 concatenate graphs
          let A.Sz1 max = A.size graph

          g <- addRandomEdges (clusterNodes * 1) max graph
          home <- next max
          delta <- next (max-1)

          return (g, home, (home + delta) `mod` max)

    let configurations = map (extractor entireGraph seed home food) [0..clusterNodes-1]

    workerAdresses <- mapM ((spawn transport) . (worker supervisorAddr)) configurations
    displayerAddr <- spawn transport displayer

    supervisor home displayerAddr workerAdresses end

  where extractor graph seed home food i =
            let snapshot = extract graph (n*i) (n*(i+1))
                A.Sz1 len = A.size graph
                table = generateTable len (n*i) (n*(i+1))
            in
              ([],snapshot,seed + i,table,home,food)

