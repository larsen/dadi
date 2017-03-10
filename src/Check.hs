{-# LANGUAGE DeriveGeneric #-}

module Check
    ( Check(..)
    , CheckState(..)
    , Collection
    , NewCheckInput(..)
    , generateCheck
    , solveCheck
    , pickSolution
    ) where

import qualified Data.Map.Strict as M
import Data.UUID
import Data.UUID.V4 (nextRandom)
import qualified System.Random as Random
import GHC.Generics
import Data.Aeson.Types


type Description = String
type Target = Int

instance ToJSON UUID where
  toJSON = toJSON . toString

data Check = Check {
  hash :: UUID,
  description :: String,
  target :: Int, -- probability
  status :: CheckState
} deriving (Eq, Show, Generic)

data CheckState
  = Open
  | Botched Int
  | Passed Int
  deriving (Eq, Show, Generic)

data NewCheckInput = NewCheckInput {
  desc :: String,
  trg :: Int
} deriving (Show, Generic)

instance ToJSON Check

instance ToJSON CheckState

instance FromJSON NewCheckInput


type Collection = M.Map UUID Check


generateCheck :: Description -> Target -> IO Check
generateCheck description target = do
  id <- nextRandom
  return Check {
    hash = id,
    description = description,
    target = target,
    status = Open
  }


solveCheck :: Check -> Int -> Check
solveCheck check@(Check _ _ target Open) solution =
  check { status = if solution <= target
                   then Passed solution
                   else Botched solution }

pickSolution :: IO Int
pickSolution = Random.getStdRandom (Random.randomR (1,100))

solveCheckIO :: Check -> IO Check
solveCheckIO check = do
  solution <- pickSolution
  return $ solveCheck check solution
