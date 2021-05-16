{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import           Data.List           (find)
import           Data.Maybe          (mapMaybe)

import           Control.Monad.ST
import           Control.Monad.State as S

import qualified Data.Massiv.Array   as A

import           Definitions
import           Random
import           Utils

newAnt home = Ant {
    vertex=home, ttl=50,
    path=[home], comingBack=False }

selectAnts ants isRemote = do

    m <- mapM f ants

    return $ map fst $ filter snd $ zip ants m

  where f :: Ant -> StateM s Bool
        f Ant{..} = do

            table <- gets table
            let loc = table A.! vertex

            return $ case loc of
              Local  -> not isRemote
              Remote -> isRemote


{- Business Logic -}

accomodate :: Ant -> StateM s ()
accomodate ant = do
    ants <- gets ants
    modify (\s@StateMachine{..} -> s { ants=ant:ants })

getDisplay :: StateM s String
getDisplay = do
    ants <- gets ants
    subgraph <- gets subgraph

    snap <- A.freezeS subgraph
    let vertices = map vertex ants

    return $ displayGraph vertices snap

move :: StateM s [Ant]
move = do

    ants <- gets ants

    g <- gets subgraph
    t <- gets table
    food <- gets food

    ants' <- forM ants $ \a@Ant{..} -> do

      let location = t A.! vertex

      case location of
        Remote -> error "Attempting to move remote ant"

        Local | ttl <= 0 -> return Nothing

        Local | comingBack && path == [] -> return Nothing

        Local | comingBack && path /= [] -> do

          let next = head path
          let loc = t A.! next

          A.modifyM g (mapM $ h next) vertex

          return $ Just a {
              comingBack=length path == 1,
              path=tail path,
              vertex=next }

        Local | vertex == food ->

          return $ Just a {
              comingBack=True }

        Local | not comingBack -> do

          neighbours <- lift $ A.readM g vertex

          let sumPheromones = sum $ map (\(_,c,_) -> c) neighbours
          rand <- generateRandom (sumPheromones - 1)

          let (_,next,cost,_) = foldl f (rand,0,0,0) neighbours

          A.modifyM g (mapM $ h next) vertex

          return $ Just a {
                path=next:path,
                ttl=ttl-1-cost,
                vertex=next }

    let ants'' = mapMaybe id ants'
        remote = selectAnts ants'' True
        local = selectAnts ants'' False

    local >>= \new -> S.modify (\st -> st { ants=new })

    remote

  where f (remain,v',c',p') (v,c,p) | remain <= 0 = (0,v',c',p')
                                 | otherwise = (remain - c,v,c,p)

        h next (v,c,p) | v == next = return (v,c,p+1)
                       | otherwise = return (v,c,p)

