module Check
    ( Check(..)
    , CheckState(..)
    , generateCheck
    , solveCheck
    ) where

import qualified Data.Map.Strict as M
import Data.UUID
import Data.UUID.V4 (nextRandom)
import qualified System.Random as Random


type Description = String
type Target = Int

data Check = Check {
  hash :: UUID,
  description :: String,
  target :: Int, -- probability
  status :: CheckState
} deriving Show

data CheckState
  = Open
  | Botched Int
  | Passed Int
  deriving Show

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


solveCheck :: Check -> IO Check
solveCheck (Check hash description target Open) = do
  solution <- Random.getStdRandom (Random.randomR (1,100))

  let newStatus = if solution <= target
                    then Passed solution
                    else Botched solution

  return Check {
    hash = hash,
    description = description,
    target = target,
    status = newStatus
  }
