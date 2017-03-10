{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Api
    (
      server
    ) where


import Check
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Servant.API
import Servant
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as M
import qualified Data.UUID as UUID
import Network.Wai
import Network.Wai.Middleware.Cors

type PostCheckRoute = "check" :> "new"
  :> ReqBody '[JSON] NewCheckInput
  :> Post '[JSON] Check

type GetCheckRoute  = "check" :> Capture "id" String :> Get '[JSON] (Maybe Check)
type PutCheckRoute  = "check" :> Capture "id" String :> Put '[JSON] Check
type ListChecksRoute = "checks" :> Get '[JSON] [Check]

type DadiAPI = PostCheckRoute
          :<|> GetCheckRoute
          :<|> PutCheckRoute
          :<|> ListChecksRoute


data DB = DB { unDB :: Collection } deriving (Show, Typeable)

-- $(deriveSafeCopy 0 'base ''DB)

insertCheck :: Check -> Update DB ()
insertCheck c = modify $ (DB . M.insert (hash c) c . unDB)

getCollection :: Query DB Collection
getCollection = ask >>= unDB

solveCheckAcid :: Check -> Int -> Update DB (Maybe Check)
solveCheckAcid c solution = do
  DB m <- get
  maybeCheck <- maybe (return Nothing) (\ k -> M.lookup k m) $ UUID.fromString hash_
  flip traverse maybeCheck $ \ c -> do
    let c' = solveCheck c solution
    modify $ DB . M.updateWithKey (\ _ _ -> Just c') (hash c) . unDB
    return c'

$(makeAcidic ''DB ['insertCheck, 'getCollection])

newCheck :: AcidDB -> Server PostCheckRoute
newCheck db (NewCheckInput { .. }) =
  if trg < 0 || trg > 100 then
    throwError err500 { errBody = "target must be between 0 and 100"}
  else do
    c <- liftIO $ generateCheck desc trg
    liftIO $ update db $ insertCheck c
    return c



checkNotFound :: ExceptT ServantErr IO a
checkNotFound = throwError err404 { errBody = "No Check found" }

getCheck :: AcidDB -> Server GetCheckRoute
getCheck db hash =
  let
    q :: UUID.UUID -> Query DB (Maybe Check)
    q k = ask >>= return $ M.lookup v . unDB
    succeed k = liftIO $ query db (q k)
  in
    maybe checkNotFound succeed $ UUID.fromString hash


putCheck :: DB -> Server PutCheckRoute
putCheck db hash_ = do
  solution <- liftIO pickSolution
  m <- liftIO $ update db (solveCheckAcid hash_)
  maybe checkNotFound return m

listCheck :: DB -> Server ListChecksRoute
listCheck db = do
  m <- liftIO $ atomically $ readTVar db
  return $ fmap snd $ M.toList m


apiServer :: DB -> Server DadiAPI
apiServer db =
  newCheck db
  :<|> getCheck db
  :<|> putCheck db
  :<|> listCheck db


userAPI :: Proxy DadiAPI
userAPI = Proxy

ourCors :: Middleware
ourCors = cors $ const (Just resourcePolicy)

resourcePolicy :: CorsResourcePolicy
resourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

server :: IO Application
server = do
  db <- openLocalState $ DB M.empty
  return $ ourCors $ serve userAPI $ apiServer db
