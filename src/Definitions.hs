{-# LANGUAGE DeriveGeneric #-}
module Definitions where

import           Data.Binary
import           GHC.Generics        (Generic)

import           Network.Transport

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State

import           Data.Massiv.Array

{- Globals -}

-- TODO: remove. Use ReaderT instead

n :: Int -- Number of vertices per node
n = 3

deltaT :: Int
deltaT = 500

{- ### -}

type NodeId = Int
type Cost = Int
type Pheromones = Int

type Edge = (Vertex,Cost,Pheromones)
type Vertex = Int
type Graph s = MArray (PrimState (ST s)) B Ix1 [Edge]
type Snapshot = Array B Ix1 [Edge]

type Resolver = Vertex -> Connection
type WorkerConfig = ([Ant], Snapshot, Int, LocationTable, Vertex, Vertex)

data Protocol = AntTransfer Ant
    | TransferRequest [Ant]
    | DisplayRequest EndPointAddress
    | DisplayResponse String
    | FlushDisplay Int
    | Step
    deriving Generic

data Location = Local
    | Remote
    deriving Show
type LocationTable = Array B Vertex Location

type StateM s = StateT (StateMachine s) (ST s)
type RandomM a = State Int a

data StateMachine s = StateMachine
    { subgraph :: Graph s
    , random   :: Int
    , table    :: LocationTable
    , food     :: Vertex
    , home     :: Vertex
    , ants     :: [Ant]
    }

data Ant = Ant
    { vertex     :: Vertex
    , ttl        :: Int
    , path       :: [Vertex]
    , comingBack :: Bool
    }
    deriving (Show, Generic)

instance Binary Ant
instance Binary Protocol

