{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Utils where

import           Control.Monad.ST
import           Control.Monad.State as S

import           Data.IORef
import           Data.List           (find, zip3)

import qualified Data.Massiv.Array   as A

import           Definitions
import           Random

{- Graph Generation -}

addRandomEdges :: Int -> Int -> Snapshot -> RandomM Snapshot
addRandomEdges n max arr = do
    let monad = mapM (const $ (,,) <$> next max <*> next max <*> next 10) [1..n]

    added <- monad

    let f idx e = case find (\(i,j,c) -> i == idx) added of
                      Just (i,j,c) -> (j,c,0):e
                      Nothing      -> e

    return $ A.computeS $ A.imap f arr

completeGraph :: State Int Snapshot
completeGraph =

      (A.fromList @A.B A.Seq) <$> m

  where m = S.forM [0..n-1] $ \i -> do

                costs <- mapM (const $ next 10) [1..n-1]

                let vertices = [ j | j <- [0..n-1], j /= i ]
                    pheromones = take (n-1) $ repeat 0

                return $ Data.List.zip3 vertices costs pheromones

generateTable :: Int -> Int -> Int -> LocationTable
generateTable len from to =
    A.makeArray A.Seq (A.Sz1 len) f
  where f idx | idx >= from = Local
              | otherwise = Remote

extract :: Snapshot -> Int -> Int -> Snapshot
extract snap from to =

    A.computeS $ A.imap f snap

  where f idx e | idx >= from && idx < to = e
                | otherwise = []

concatenate :: Snapshot -> Snapshot -> Snapshot
concatenate a b =
    let arr = A.computeS @A.B $ A.concat' (A.Dim 1)[a,b]
        A.Sz1 len = A.size a
    in
      A.computeS $ A.imap (f len) arr

  where f len idx e | idx >= len = map (\(v,c,p) -> (v+len,c,p)) e
                    | otherwise = e

{- Easier type checking -}

createMArray :: (A.Mutable r idx e) => A.Array r idx e
                     -> ST s (A.MArray (A.PrimState (ST s)) r idx e)
createMArray arr = A.thawS arr


{- Monad Runners -}

runWorker :: WorkerConfig -> IORef (Maybe WorkerConfig)
             -> (forall s. StateM s a) -> IO a
runWorker config ref monad = do
    val <- readIORef ref

    case val of
      Nothing -> do
          let (config',res) = runST (runLocal config (withConfig monad))
          writeIORef ref (Just config')

          return res

      Just config' -> do
          let (config'',res) = runST (runLocal config' (withConfig monad))
          writeIORef ref (Just config'')

          return res

  where withConfig :: StateM s a -> StateM s (WorkerConfig,a)
        withConfig monad = do
            res <- monad
            StateMachine{..} <- get
            g <- A.freezeS subgraph
            let c = (ants,g,random,table,home,food) :: WorkerConfig
            return (c,res)


runLocal :: WorkerConfig -> StateM s a -> ST s a

runLocal (ants,pregraph,seed,table,home,food) monad = do
    g <- createMArray pregraph

    let state = StateMachine {
          subgraph=g, random=seed,
          table, food, home, ants }

    evalStateT monad state

{- Rendering -}

displayGraph :: [Vertex] -> Snapshot -> String
displayGraph colored g = A.ifoldMono showEdge g

  where render v (v',p,c) =

                  let color = "black" -- TODO
                      label = show p ++ ";" ++ show c in

              show v ++ " -> " ++ show v' ++ " [color=" ++ color ++ ",label=\""
                ++ label ++ "\"]"

        showEdge v edges = "\n\n" ++ (unlines $ map (render v) edges)

        vertexColor v | v `elem` colored = "blue"
                      | otherwise = "black"

